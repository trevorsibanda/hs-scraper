{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# Language BlockArguments #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
module DataLake.DataLake where

import Control.Monad (when, void, forM, forM_)
import Control.Monad.IO.Class
import UnliftIO.STM
import UnliftIO

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

import Model

data CrawlStatus = CrawlStatus
  { pagesCrawled :: Int,
    resourcesCrawled :: Int,
    resourcesFailed :: Int,
    resourcesRetried :: Int
  }
  deriving (Show, Eq)


-- | A data lake is a place where crawled data is stored.
data DataLake m a = DataLake{

    addResource :: a -> m (),
    getLinkStatus :: URL -> m (Maybe URLCrawlState),
    
    setLinkRetry :: URL -> Int -> m Bool,
    setLinkFailed :: URL -> Text -> m Bool,
    setLinkVisited :: URL -> Bool -> m Bool,

    status :: IO CrawlStatus,

    popLinkToVisit :: m URL,
    tryPopLinkToVisit :: m (Maybe URL),
    popLinkToRetry :: m URL,
    tryPopLinkToRetry :: m (Maybe URL),

    addLinkToVisit :: URL -> m Bool,
    addLinksToVisit :: [URL] -> m Int,
    addLinksToVisitPred :: [URL] -> (M.Map URL URLCrawlState -> URL -> Bool) -> m Int,

    getVisitedLinks :: m (S.Set URL),
    getLinksPred :: (URLCrawlState -> Bool) -> m [(URL, URLCrawlState)],
    getResources :: m( M.Map Text a),
    getPendingLinks :: m [URL],
    getRetryLinks :: m [URL], -- TODO: change to (URL, Int)
    getFailedLinks :: m [(URL, Text)],

    isLinkVisited :: URL -> m Bool,
    isLinkPending :: URL -> m Bool,
    isLinkFailed :: URL -> m Bool,
    isLinkRetried :: URL -> m Bool,
    isLinkPred :: URL -> (URLCrawlState -> Bool) -> m Bool

  }

data URLCrawlState = Visited Bool | Pending | Failed (Text, Int) | Retrying Int | UnknownCrawlState Text deriving (Show, Eq)
instance FromField URLCrawlState where
    fromField f mdata = do
        bs <- fromField f mdata
        case bs of
            Nothing -> returnError UnexpectedNull f ""
            Just bs' -> pure $ rowTextToCrawlState $ T.pack $ BS.unpack bs'

instance ToField URLCrawlState where
    toField = toField . crawlStateToRowText

crawlStateToRowText :: URLCrawlState -> Text
crawlStateToRowText = \case 
    Visited True -> "resource"
    Visited False -> "visited"
    Pending -> "pending"
    Failed _ -> "failed"
    Retrying _ -> "retrying"

rowTextToCrawlState :: Text -> URLCrawlState
rowTextToCrawlState = \case 
    "resource" -> Visited True
    "visited" -> Visited False
    "pending" -> Pending
    "failed" -> Failed ("", 0)
    "retrying" -> Retrying 0
    t -> UnknownCrawlState t

