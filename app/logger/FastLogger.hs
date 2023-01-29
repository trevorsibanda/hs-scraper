{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
module Logger.FastLogger (
    module Logger.Logger,
    newSafeConsoleLogger,
    newSafeSequentialLogger
)where

import Logger.Logger
import UnliftIO
import UnliftIO.Concurrent
import Control.Monad (forever)
import qualified UnliftIO.STM  as STM

import Data.Text (Text)
import qualified Data.Text as T



newSafeSequentialLogger :: (MonadUnliftIO m, MonadUnliftIO n) => Logger m -> m (Logger n)
newSafeSequentialLogger classicLogger = do
    tqueue <- STM.atomically $ STM.newTQueue @(LogLevel, Text)
    mv <- newMVar True
    tID <- forkIO $  forever $ do
        (lvl, msg) <-  STM.atomically $ STM.readTQueue tqueue
        _ <- takeMVar mv
        runLogger classicLogger lvl msg
        putMVar mv True

    pure Logger{
        runLogger = \lv msg -> STM.atomically $ STM.writeTQueue tqueue (lv, msg)
    }

newSafeConsoleLogger :: (MonadUnliftIO m, MonadUnliftIO n) => m (Logger n)
newSafeConsoleLogger = do
    classicLogger <- newSingleThreadConsoleLogger
    newSafeSequentialLogger classicLogger