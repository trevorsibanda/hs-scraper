{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# Language BlockArguments #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
module DataLake.Postgres (module DataLake.DataLake, newPostgresDataLake) where

import DataLake.DataLake
import Control.Monad (when, void, forM, forM_)
import Control.Monad.IO.Class
import UnliftIO.STM
import UnliftIO
import UnliftIO.Concurrent
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.FromField

import Model.Serde
import Model.Resource

bootstrapStmts :: [Query]
bootstrapStmts = [
    "CREATE TABLE IF NOT EXISTS links (url TEXT PRIMARY KEY, status TEXT, retry_count INTEGER, error TEXT)",
    "CREATE INDEX IF NOT EXISTS links_status_idx ON links (status)",
    -- See Model.PropertyListing FromRow instance for the reason for this table
    "CREATE TABLE IF NOT EXISTS resources (url TEXT PRIMARY KEY, title TEXT, description TEXT, image TEXT, content TEXT)"
    ]

newPostgresDataLake :: (MonadUnliftIO m, MonadUnliftIO n, FromRow a, ToRow a, HasLookupKey a) => Text -> n (DataLake m a)
newPostgresDataLake connString = do
    let connStringBS = BS.pack $ T.unpack connString
    conn <- liftIO $ connectPostgreSQL connStringBS
    bootstrap conn
    pure DataLake{
        addResource = addResource conn,
        getLinkStatus = getLinkStatus conn,
        setLinkRetry = setLinkRetry conn,
        setLinkFailed = setLinkFailed conn,
        setLinkVisited = setLinkVisited conn,
        addLinksToVisitPred = addLinksToVisitPred conn,
        addLinkToVisit = addLinkToVisit conn,
        addLinksToVisit = addLinksToVisit conn,
        getLinksPred = getLinksPred conn,
        getVisitedLinks = getVisitedLinks conn,
        getPendingLinks = getPendingLinks conn,
        getRetryLinks = getRetryLinks conn,
        getFailedLinks = getFailedLinks conn,
        isLinkPred = isLinkPred conn,
        isLinkVisited = isLinkVisited conn,
        isLinkPending = isLinkPending conn,
        isLinkFailed = isLinkFailed conn,
        isLinkRetried = isLinkRetried conn,
        status = status conn,
        tryPopLinkToRetry = tryPopLinkToRetry conn,
        tryPopLinkToVisit = tryPopLinkToVisit conn,
        popLinkToRetry = popLinkToRetry conn,
        popLinkToVisit = popLinkToVisit conn,
        getResources = getResources conn
    }
    where
    bootstrap conn = liftIO $ forM_ bootstrapStmts (\stmt -> void $ execute_ conn stmt)
    addResource :: (MonadUnliftIO m, ToRow a) => Connection  -> a -> m ()
    addResource conn v = do
        liftIO $ void $ execute conn "INSERT INTO resources (url, title, description, image, content) VALUES (?,?,?,?,?)" v

    getLinkStatus :: (MonadUnliftIO m) =>  Connection -> URL -> m (Maybe URLCrawlState)
    getLinkStatus conn url = do
        res <- liftIO ( query conn "SELECT status FROM links WHERE url = ?" (Only url) :: IO [Only URLCrawlState])
        case res of
            [Only (status :: URLCrawlState)] -> pure $ Just  status
            _ -> pure Nothing
    
    setLinkRetry :: (MonadUnliftIO m) => Connection  -> URL -> Int -> m Bool
    setLinkRetry conn url retryCount = do
        res <- liftIO $ execute conn "UPDATE links SET status = ?, retry_count = ? WHERE url = ?" (Retrying retryCount, retryCount, url)
        pure $ res == 1

    setLinkFailed :: (MonadUnliftIO m) => Connection  -> URL -> Text -> m Bool
    setLinkFailed conn url reason = do
        res <- liftIO $ execute conn "UPDATE links SET status = ?, reason = ? WHERE url = ?" (Failed ("", 0), reason, url)
        pure $ res == 1


    setLinkVisited :: (MonadUnliftIO m) => Connection  -> URL -> Bool -> m Bool
    setLinkVisited conn url success = do
        res <- liftIO $ execute conn "UPDATE links SET status = ? WHERE url = ?" (Visited success, url)
        pure $ res == 1

    status :: Connection -> IO CrawlStatus
    status conn = do
        res <- query_ conn "SELECT status, count(*) FROM links GROUP BY status" :: IO [(URLCrawlState, Int)]
        pure $ foldl (\(CrawlStatus v p r f) (status :: URLCrawlState, count :: Int) -> case status of
            Visited _ -> CrawlStatus (v + count) p r f
            Pending -> CrawlStatus v (p + count) r f
            Retrying _ -> CrawlStatus v p (r + count) f
            Failed _ -> CrawlStatus v p r (f + count)
            ) (CrawlStatus 0 0 0 0) res

    popLinkToVisit :: (MonadUnliftIO m) => Connection  -> m URL
    popLinkToVisit conn = do
        res <- liftIO $ query_ conn "SELECT url FROM links WHERE status = 1 LIMIT 1"
        case res of
            [Only (url :: URL)] -> do
                liftIO $ execute conn "UPDATE links SET status = 2 WHERE url = ?" (Only url)
                pure url
            _ -> do
                threadDelay 1000000
                popLinkToVisit conn

    tryPopLinkToVisit :: (MonadUnliftIO m) => Connection  -> m (Maybe URL)
    tryPopLinkToVisit conn = do
        res <- liftIO $ query_ conn "SELECT url FROM links WHERE status = 1 LIMIT 1"
        case res of
            [Only (url :: URL)] -> do
                liftIO $ execute conn "UPDATE links SET status = 2 WHERE url = ?" (Only url)
                pure $ Just url
            _ -> do
                pure Nothing


    popLinkToRetry :: (MonadUnliftIO m) => Connection  -> m URL
    popLinkToRetry conn = do
        res <- liftIO $ query_ conn "SELECT url FROM links WHERE status = \"retry\" LIMIT 1"
        case res of
            [Only (url :: URL)] -> do
                liftIO $ execute conn "UPDATE links SET status = \"retry\" WHERE url = ?" (Only url)
                pure url
            _ -> do
                threadDelay 1000000
                popLinkToRetry conn

    tryPopLinkToRetry :: (MonadUnliftIO m) => Connection  -> m (Maybe URL)
    tryPopLinkToRetry conn = do
        res <- liftIO $ query_ conn "SELECT url FROM links WHERE status = \"retry\" LIMIT 1"
        case res of
            [Only (url :: URL)] -> do
                liftIO $ execute conn "UPDATE links SET status = 2 WHERE url = ?" (Only url)
                pure $ Just url
            _ -> do
                pure Nothing

    addLinkToVisit :: (MonadUnliftIO m) => Connection  -> URL -> m Bool
    addLinkToVisit conn url = do
        res <- liftIO $ execute conn "INSERT INTO links (url, status) VALUES (?, ?) ON CONFLICT DO NOTHING" (url, Pending)
        pure $ res == 1

    addLinksToVisit :: (MonadUnliftIO m) => Connection  -> [URL] -> m Int
    addLinksToVisit conn links = do
        res <- liftIO $ executeMany conn "INSERT INTO links (url, status) VALUES (?, ?) ON CONFLICT DO NOTHING" $ map (\url -> (url, Pending)) links
        pure $ fromIntegral res

    addLinksToVisitPred :: (MonadUnliftIO m) => Connection  -> [URL] -> (M.Map URL URLCrawlState -> URL -> Bool) -> m Int
    addLinksToVisitPred conn links pred = do
        res <- liftIO ( query_ conn "SELECT url, status FROM links" :: IO [(URL, URLCrawlState)])
        let linkMap = M.fromList res
        let linksToVisit = filter (pred linkMap) links
        addLinksToVisit conn linksToVisit

    getVisitedLinks :: (MonadUnliftIO m) => Connection  -> m (S.Set URL)
    getVisitedLinks conn = do
        res <- liftIO $ query_ conn "SELECT url FROM links WHERE status = \"visited\""
        pure $ S.fromList $ map (\(Only (url :: URL)) -> url) res

    getLinksPred :: (MonadUnliftIO m) => Connection  -> (URLCrawlState -> Bool) -> m [(URL, URLCrawlState)]
    getLinksPred conn pred = do
        res <- liftIO $ query_ conn "SELECT url, status FROM links"
        pure $ filter (\(_, status) -> pred status) $ map (\(url :: URL, status :: Text) -> (url, rowTextToCrawlState status)) res

    getResources :: forall a m. (MonadUnliftIO m, FromRow a, HasLookupKey a) => Connection  -> m( M.Map Text a)
    getResources conn = do
        res <- liftIO ( query_ conn "SELECT * FROM resources" :: IO [a])
        pure $ M.fromList $ map (\(resource :: a) -> (lookupKey resource, resource)) res

    getPendingLinks :: (MonadUnliftIO m) => Connection  -> m [URL]
    getPendingLinks conn = do
        res <- liftIO $ query_ conn "SELECT url FROM links WHERE status = \"pending\""
        pure $ map (\(Only (url :: URL)) -> url) res
    
    getRetryLinks :: (MonadUnliftIO m) => Connection  -> m [URL]
    getRetryLinks conn = do
        res <- liftIO $ query_ conn "SELECT url FROM links WHERE status = \"retry\""
        pure $ map (\(Only (url :: URL)) -> url) res

    getFailedLinks :: (MonadUnliftIO m) => Connection  -> m [(URL, Text)]
    getFailedLinks conn = do
        res <- liftIO $ query_ conn "SELECT url, reason FROM links WHERE status = \"failed\""
        pure $ map (\(url :: URL, reason :: Text) -> (url, reason)) res

    isLinkVisited :: (MonadUnliftIO m) => Connection  -> URL -> m Bool
    isLinkVisited conn url = do
        res <- liftIO $ query conn "SELECT status FROM links WHERE url = ?" (Only url)
        case res of
            [Only (status :: Int)] -> pure $ status == 2
            _ -> pure False

    isLinkPending :: (MonadUnliftIO m) => Connection  -> URL -> m Bool
    isLinkPending conn url = do
        res <- liftIO $ query conn "SELECT status FROM links WHERE url = ?" (Only url)
        case res of
            [Only (status :: Int)] -> pure $ status == 1
            _ -> pure False

    isLinkFailed :: (MonadUnliftIO m) => Connection  -> URL -> m Bool
    isLinkFailed conn url = do
        res <- liftIO $ query conn "SELECT status FROM links WHERE url = ?" (Only url)
        case res of
            [Only (status :: Int)] -> pure $ status == 4
            _ -> pure False


    isLinkRetried :: (MonadUnliftIO m) => Connection  -> URL -> m Bool
    isLinkRetried conn url = do
        res <- liftIO $ query conn "SELECT status FROM links WHERE url = ?" (Only url)
        case res of
            [Only (status :: Int)] -> pure $ status == 3
            _ -> pure False

    isLinkPred :: (MonadUnliftIO m) => Connection  -> URL -> (URLCrawlState -> Bool) -> m Bool
    isLinkPred conn url pred = do
        res <- liftIO ( query conn "SELECT status FROM links WHERE url = ?" (Only url)  :: IO [Only URLCrawlState])
        case res of
            [Only status] -> pure $ pred status
            _ -> pure False
