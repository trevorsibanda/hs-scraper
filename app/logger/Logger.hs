{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Logger.Logger where

import UnliftIO
import UnliftIO.Concurrent
import Control.Monad (forever)
import qualified UnliftIO.STM  as STM
import Control.Monad.IO.Class
import Data.Text (Text)
import qualified Data.Text as T

data LogLevel = Debug | Info | Warn | Error deriving (Show, Eq)

data LogFilter = Only LogLevel | AnyOf [LogLevel] | Except LogLevel | All deriving (Show, Eq) 

newtype Logger m = Logger {runLogger :: LogLevel -> Text -> m ()}

class SweetLogger l m a  where
    logInfo :: l m -> a -> m ()
    logWarn :: l m -> a -> m ()
    logError :: l m ->a -> m ()
    logDebug :: l m -> a -> m ()

instance SweetLogger Logger m Text where
    logInfo  Logger{..} = runLogger Info 
    logWarn  Logger{..} = runLogger Warn
    logError Logger{..} = runLogger Error
    logDebug Logger{..} = runLogger Debug

instance SweetLogger Logger m String where
    logInfo  Logger{..} s = runLogger Info  $ T.pack s 
    logWarn  Logger{..} s = runLogger Warn  $ T.pack s
    logError Logger{..} s = runLogger Error $ T.pack s
    logDebug Logger{..} s = runLogger Debug $ T.pack s



newSingleThreadConsoleLogger :: (MonadUnliftIO m, MonadUnliftIO n) => m (Logger n)
newSingleThreadConsoleLogger = 
    pure Logger{..}
    where
        runLogger lv msg = liftIO $ do
            tid <- myThreadId
            liftIO $ putStrLn $ "<" <> show lv <> "> [" <> show tid <> "] "<> T.unpack msg

