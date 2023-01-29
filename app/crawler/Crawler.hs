
module Crawler.Crawler where

import Logger.Logger
import Control.Monad (forever, void)
import Control.Monad.IO.Class
import Data.Text (Text)
import Data.Default
import Data.Foldable
import qualified Data.Set as S

import qualified Data.Text as T
import qualified Data.Map as M
import qualified Data.ByteString.Char8 as BS
import Model
import Network.HTTP.Conduit.Downloader
import UnliftIO
import UnliftIO.Concurrent
import UnliftIO.STM
import Scraper.Scraper
import DataLake.DataLake
import Cache.Cache
import Model.Serde

import Database.PostgreSQL.Simple



-- | The Crawler is the main entry point for the application. It is responsible for
-- | orchestrating the scraping process.
data Crawler m a = Crawler
  { _scraper :: WebScraper m a,
    _startUrl :: TVar URL,
    _logger :: Logger m,
    _cache :: Cache m HtmlPage,
    _downloader :: Downloader,
    _config :: CrawlerConfig,
    _dataLake :: DataLake m a,
    _lastStatus :: TVar CrawlStatus,
    stopCrawler :: m ()
  }

-- | CrawlerError is used to indicate errors that occur during the crawling process
data CrawlerError = RedirectNotPermitted | TooManyRedirects | DownloadFailed Text | OtherCrawlerError Text  deriving (Show, Eq)


-- | The CrawlerConfig contains all the configuration options for the crawler
data CrawlerConfig = CrawlerConfig
  { 
    showProgress :: Bool,
    useCache :: Bool,
    progressIntervals :: Int,
    predicate :: Resource -> Bool,
    maxPages :: Int,
    maxResources :: Int,
    maxRedirects :: Int,
    followRedirects :: Bool,
    followExternalSites :: Bool,

    maxWorkers :: Int,
    maxRetries :: Int,
    maxTimeout :: Int,
    userAgent :: Text,
    additionalHeaders :: [(Text, Text)]
  }

