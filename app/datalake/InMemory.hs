{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# Language BlockArguments #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
module DataLake.InMemory(module DataLake.DataLake, newInMemoryDataLake ) where

import Data.Text (Text)
import qualified Data.Text as T

import qualified Data.Set as S

import Data.Maybe

import DataLake.DataLake
import UnliftIO
import Control.Monad.IO.Class
import Control.Monad
import qualified Data.Map as M
import UnliftIO.STM
import UnliftIO.Concurrent
import Model.Serde
import Model.Resource

-- | A data lake that stores data in memory.
newInMemoryDataLake :: forall (m :: * -> *)  a. (HasLookupKey a, MonadUnliftIO m) => m (DataLake m a)
newInMemoryDataLake = do
    linksM <-  newTVarIO $ M.empty @URL @URLCrawlState
    extractedResources <-  newTVarIO $ emptyResources
    queue <-  newTQueueIO
    retryQueue <-  newTQueueIO


    pure $ DataLake{  
        -- | Add a resource to the list of extracted resources.
        addResource = \res ->   atomically $ modifyTVar extractedResources (M.insert (lookupKey res) res),

        getResources =  atomically $ readTVar extractedResources,

        getLinkStatus = getLinkStatus' linksM,

        status = do
            allLinks <- atomically $ readTVar linksM
            visited <-  atomically $ newTVar 0
            pending <-  atomically $ newTVar 0
            failed <-  atomically $ newTVar 0
            retried <-  atomically $ newTVar 0
            void $ forM (M.toList allLinks) $ \case
                (url, Visited _) ->  atomically $ modifyTVar visited (+1)
                (url, Pending) ->  atomically $ modifyTVar pending (+1)
                (url, Failed _) ->  atomically $ modifyTVar failed (+1)
                (url, Retrying _) ->  atomically $ modifyTVar retried (+1)
            CrawlStatus <$> (atomically $ readTVar visited)
                        <*> (atomically $ readTVar extractedResources >>= pure . M.size)
                        <*> (atomically $ readTVar failed)
                        <*> (atomically $ readTVar retried),

        popLinkToVisit =  atomically $ readTQueue queue,
        tryPopLinkToVisit =  atomically $ tryReadTQueue queue,

        popLinkToRetry =  atomically $ readTQueue retryQueue,
        tryPopLinkToRetry =  atomically $ tryReadTQueue retryQueue,

        setLinkRetry = \url count -> do
            ls <- getLinkStatus' linksM url
            case ls of
                Just (Retrying count) -> insertIf (const. const True) url (Retrying (count+1)) linksM
                Just (Pending) -> insertIf (const.const True) url (Retrying 1) linksM
                _ -> pure False,
        setLinkFailed = \url failure -> do
            ls <- getLinkStatus' linksM url
            case ls of
                Just (Retrying count) -> insertIf (const.const True) url (Failed (failure, count+1)) linksM
                Just (Pending) -> insertIf (const.const True) url (Failed (failure, 1)) linksM
                _ -> pure False,

        setLinkVisited =  \url parsed -> insertIf (const $ \case Pending -> True; Retrying _ -> True; _ -> False ) url (Visited parsed) linksM,
        
        addLinksToVisitPred = addLinksToVisitPred' linksM queue,
            

        addLinkToVisit = \url -> do
            count <- addLinksToVisitPred' linksM queue [url] canVisit
            pure $ count == 1,

        addLinksToVisit = \urls -> addLinksToVisitPred' linksM queue urls (\links url -> isNothing $ M.lookup url links),

        getLinksPred = getLinksPred' linksM,

        getVisitedLinks = do
             links <- getLinksPred' linksM (\case Visited _ -> True; _ -> False)
             pure $ S.fromList $ fst <$> links,
        getPendingLinks = do
            links <- getLinksPred' linksM (\case Pending -> True; _ -> False)
            pure $ fst <$> links,
        getRetryLinks = do
            links <- getLinksPred' linksM (\case Retrying _ -> True; _ -> False)
            pure $ fst <$>links,
        getFailedLinks = do
            links <- getLinksPred' linksM (\case Failed _ -> True; _ -> False)
            pure $ (\(l, Failed (r,c)) -> (l, r)) <$> links,


        isLinkPred = isLinkPred' linksM,
        isLinkVisited = \url -> isLinkPred' linksM  url (\case Visited _ -> True; _ -> False),
        isLinkPending = \url -> isLinkPred' linksM url (\case Pending -> True; _ -> False),
        isLinkFailed = \url ->  isLinkPred' linksM url (\case Failed _ -> True; _ -> False),
        isLinkRetried = \url -> isLinkPred' linksM url (\case Retrying _ -> True; _ -> False)

    }
    where
        getLinkStatus' :: (MonadUnliftIO m) => TVar (M.Map URL URLCrawlState) -> URL -> m (Maybe URLCrawlState)
        getLinkStatus' linksM url = do
            links <-  atomically $ readTVar linksM
            pure $ M.lookup url links

        isLinkPred'  :: (MonadUnliftIO m) =>  TVar (M.Map URL URLCrawlState) -> URL -> (URLCrawlState -> Bool) -> m Bool
        isLinkPred' linksM url p = do
            links <-  atomically $ readTVar linksM
            pure $ isJust $ M.lookup url $ M.filterWithKey (\_ cs -> p cs) links

        addLinksToVisitPred'  :: (MonadUnliftIO m) =>  TVar (M.Map URL URLCrawlState) -> TQueue URL -> [URL] -> (M.Map URL URLCrawlState -> URL -> Bool) -> m Int
        addLinksToVisitPred' linksM queue urls p = do
            links <- atomically $ readTVar linksM
            let toAdd = filter (p links) ((T.strip . T.toLower) <$> urls)
                updatedLinks = foldl (\m url -> M.insert url Pending m) links toAdd
            atomically $ writeTVar linksM updatedLinks 
            forM_ toAdd $ \url -> do
                case (M.member url links) of
                    True ->
                        pure ()
                    False -> do
                        -- print updatedLinks
                        atomically $ writeTQueue queue url
            pure $ length toAdd

        getLinksPred'  :: (MonadUnliftIO m) =>  TVar (M.Map URL URLCrawlState) -> (URLCrawlState -> Bool) -> m [(URL, URLCrawlState)]
        getLinksPred' linksM p = do
            links <-  atomically $ readTVar linksM
            pure $ M.toList $ M.filterWithKey (\_ cs -> p cs) links

        emptyResources :: M.Map Text a
        emptyResources = M.empty

        insertIf :: (MonadUnliftIO m) => (URL -> URLCrawlState -> Bool) -> URL -> URLCrawlState -> TVar (M.Map URL URLCrawlState) -> m Bool
        insertIf p url crawlState m = do
            mv <-  atomically $ readTVar m
            case M.lookup url mv of
                Just ls -> do 
                    when (p url ls) ( atomically $  modifyTVar m (M.insert url crawlState) )
                    pure True
                Nothing -> do
                    pure False

        canVisit links url = case M.lookup url links of
                Just (Visited _) -> False
                Just (Pending) -> False
                Just (Failed _) -> False
                Just (Retrying _) -> False
                Nothing -> True
        
        canRetry links url = case M.lookup url links of
                Just (Retrying _) -> True
                _ -> False
