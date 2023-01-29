{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE  LambdaCase #-}
module Crawler.Simple (
  module Crawler.Crawler,
  crawl,
  runCrawler,
  newSimpleCrawler,
  newPersistentCrawler,
  newPersistentCrawler',
  logCrawlerProgress,
  defaultConfig,
  downloadHtmlPage,
  startConsoleStatusLogger,

  
) where

import Crawler.Crawler
import Control.Monad (forever, void)
import Control.Monad.IO.Class
import Data.Text (Text)
import Data.Default
import Data.Foldable
import qualified Data.Set as S

import qualified Data.Text as T
import qualified Data.Map as M
import qualified Data.ByteString.Char8 as BS
import Model.Serde
import Model.Resource
import Network.HTTP.Conduit.Downloader
import UnliftIO
import UnliftIO.Concurrent
import UnliftIO.STM
import Scraper.Scraper
import DataLake.DataLake
import DataLake.InMemory (DataLake, newInMemoryDataLake)
import DataLake.Postgres (newPostgresDataLake)
import Cache.InMemory (Cache, newInMemoryCache)
import qualified Cache.InMemory as Cache
import Cache.Postgres (newPostgresCache)
import Logger.FastLogger

import Database.PostgreSQL.Simple(FromRow, ToRow)


-- | Create a new crawler with in memory cache and console logger
newSimpleCrawler :: (MonadUnliftIO m, Serde a, HasLookupKey a) => WebScraper m a -> CrawlerConfig -> m (Crawler m a)
newSimpleCrawler _scraper _config = do
  _cache <- newInMemoryCache
  _downloader <- liftIO $ newDownloader defaultDownloaderConfig
  _logger <- newSafeConsoleLogger
  _dataLake <- newInMemoryDataLake
  newCrawler _scraper _dataLake _config _logger _downloader _cache

-- | SCreate a new crawler
newCrawler :: (MonadUnliftIO n, MonadUnliftIO m, Serde a) => WebScraper m a -> DataLake m a -> CrawlerConfig -> Logger m -> Downloader -> Cache m HtmlPage -> n (Crawler m a)
newCrawler _scraper _dataLake _config _logger _downloader _cache = do
  _lastStatus <- atomically $ newTVar $ CrawlStatus 0 0 0 0 
  _startUrl <- atomically $ newTVar ""
  pure $ Crawler {..}



-- | Persistent crawler with postgres datalake and postgres cache
newPersistentCrawler :: (MonadUnliftIO n, MonadUnliftIO m, Serde a, FromRow a, ToRow a, HasLookupKey a) => WebScraper m a -> DataLake m a -> CrawlerConfig -> Text -> n (Crawler m a)
newPersistentCrawler _scraper _dataLake _config postgresConnString = do
  _cache <- newPostgresCache postgresConnString
  _downloader <- liftIO $ newDownloader defaultDownloaderConfig
  _logger <- newSafeConsoleLogger
  _dataLake <- newPostgresDataLake postgresConnString
  newCrawler _scraper _dataLake _config _logger _downloader _cache

-- | Persistent crawler with postgres datalake and in memory cache
newPersistentCrawler' :: (MonadUnliftIO n, MonadUnliftIO m, Serde a, FromRow a, ToRow a, HasLookupKey a) => WebScraper m a -> DataLake m a -> CrawlerConfig -> Text -> n (Crawler m a)
newPersistentCrawler' _scraper _dataLake _config postgresConnString = do
  _cache <- newInMemoryCache
  _downloader <- liftIO $ newDownloader defaultDownloaderConfig
  _logger <- newSafeConsoleLogger
  _dataLake <- newPostgresDataLake postgresConnString
  newCrawler _scraper _dataLake _config _logger _downloader _cache


-- | The CrawlStatus contains the current status of the crawler
logCrawlerProgress :: (MonadUnliftIO m) => Crawler m a -> m ()
logCrawlerProgress Crawler {..} = do
  case showProgress _config of
    True -> do
      status <- liftIO $ status _dataLake
      logInfo _logger $ "Progress:  " ++ show status
    False -> pure ()


defaultDownloaderConfig :: DownloaderSettings
defaultDownloaderConfig = def

startConsoleStatusLogger :: (MonadUnliftIO m) => Crawler m a -> m ()
startConsoleStatusLogger c@Crawler {..} = do
  myTid <- myThreadId
  tid <- atomically $ newTVar myTid
  tid' <- forkIO $ forever $ do
    threadDelay $ progressIntervals _config * 1000000
    logCrawlerProgress c
    
  void $ atomically $ writeTVar tid tid'



runCrawler :: (MonadUnliftIO m, MonadFail m, Show a) => Crawler m a -> URL -> m ()
runCrawler c@Crawler{..} url = do
  b <- addLinkToVisit _dataLake url
  if b
    then do
      logInfo _logger $ "Added link to visit : " <> (show url)
    else do
      logWarn _logger $ "Failed to add link to visit, probably already visitted : " <> (show url)
  for_ [1.. (maxWorkers _config)] $ \idx -> do 
    tID <- forkIO $ crawl c isInternalLink  url
    logInfo _logger $ "Starting crawler worker: #" <> show idx <> " on Thread "<> show tID


 where 
  localAuthority = parseURLAuthority url 

  isInternalLink :: URL ->  Bool
  isInternalLink url = parseURLAuthority url  == localAuthority


crawl :: (MonadUnliftIO m, MonadFail m, Show a) =>  Crawler m a -> (URL ->  Bool) -> URL ->  m ()
crawl c@Crawler{..} isInternalLink startUrl = forever $ do
  logInfo _logger "Crawling... waiting for pending link"
  url <- popLinkToVisit _dataLake
  b <- isLinkVisited _dataLake url
  if(b) then do
    logInfo _logger $ "Link already visited: " <> (show url)
  else do
    crawlPage url
  where
    crawlPage url = do  
      logInfo _logger $ "Crawling " ++ T.unpack url
      page' <- downloadHtmlPage url Nothing 0 c
      case page' of
        Right page -> do
          logCrawlerProgress c
          let allLinks = extractLinks _scraper (html page)
              filteredLinks = (case followExternalSites  _config of
                True -> allLinks
                False -> S.filter isInternalLink allLinks)

          --liftIO $ putStrLn $ "Found " ++ show (length filteredLinks) ++ " new links out of " ++ show (length allLinks) ++ " total links."
          c <- addLinksToVisit  _dataLake (S.toList filteredLinks)
          logDebug _logger $ "Added " ++ show c ++ " new links out of " ++ show (length filteredLinks) ++ " total links."

          scraped <- runScraper _scraper (Html page)
          case scraped of
            Left err -> do
              setLinkVisited _dataLake url False
              logWarn _logger $ "Failed to scrape page: " ++ show err
            Right listing -> do
              setLinkVisited _dataLake url True
              addResource _dataLake listing
              logInfo _logger $ "Found\t " <> (show listing)
        Left err -> do
          logError _logger $ "Failed to download page: " ++ show err
          void $ setLinkFailed _dataLake url (T.pack $ show err)

defaultConfig ::  CrawlerConfig
defaultConfig = CrawlerConfig
  { predicate = const True,
    showProgress = True,
    useCache = False,
    progressIntervals = 2500,
    maxPages = 100,
    maxResources = 1000,
    maxRedirects = 5,
    followRedirects = True,
    followExternalSites = False,
    maxWorkers = 10,
    maxRetries = 3,
    maxTimeout = 10000,
    userAgent = "Mozilla/5.0 (X11; Linux x86_64; rv:78.0) Gecko/20100101 Firefox/78.0",
    additionalHeaders = []
  }

downloadHtmlPage :: (MonadUnliftIO m, MonadFail m) => URL -> Maybe Text -> Int -> Crawler m a -> m (Either CrawlerError HtmlPage)
downloadHtmlPage url lastAccessed redirectCount c@Crawler{..} = do
    if useCache _config then do
      page <- Cache.get _cache url
      case page of
        Just page -> parseDownload $ DROK ( BS.pack $ T.unpack $ html page) []
        Nothing -> downloadAndParse
    else
      downloadAndParse
  
  where
    downloadAndParse = do
      res <- liftIO $ download _downloader (T.unpack url) Nothing []
      parseDownload res
    parseDownload = \case
      DROK htmlbs opts -> do 
        let html = T.pack $ BS.unpack htmlbs
        let hpage = newHTMLPage url html 
        Cache.put _cache url $ hpage{links = extractLinks  _scraper html}
        pure $  Right hpage
      DRRedirect redir -> do
        logDebug _logger $ "Redirecting " <> show url <> " to " <> (show redir)
        case followRedirects _config of
          True -> do
            if(redirectCount < maxRedirects _config) then 
              downloadHtmlPage (T.pack redir) lastAccessed (redirectCount + 1) c 
            else 
              pure $ Left TooManyRedirects
          False -> pure $ Left RedirectNotPermitted
      DRError err -> pure $ Left $ DownloadFailed (T.pack err)
      DRNotModified -> pure $ Left $ OtherCrawlerError "Page not modified"

pageFromHTML :: (MonadUnliftIO m) => URL ->Text -> WebScraper m a -> m HtmlPage
pageFromHTML url html scraper' = do
  let h = newHTMLPage url html
  pure $ h {links = extractLinks  scraper' html}
