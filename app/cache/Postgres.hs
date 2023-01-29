{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Cache.Postgres(module Cache.Cache, newPostgresCache) where

import Cache.Cache
import Data.Text (Text)

import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map as M
import UnliftIO
import Model.Serde
import qualified UnliftIO.STM as STM
import Control.Monad
import Control.Monad.IO.Class

import Database.PostgreSQL.Simple



newPostgresCache :: (MonadUnliftIO m, MonadUnliftIO n, Show a, Serde a) => Text -> n (Cache m a)
newPostgresCache connectionString' = do
    let connectionString = BS.pack $ T.unpack connectionString'
    conn <- liftIO $ connectPostgreSQL connectionString
    bootStrap conn
    pure Cache{
        cacheType = PostgresCache connectionString',
        get = get conn,
        put = put conn,
        delete = delete conn,
        keys = keys conn,
        clear = clear conn,
        stats = stats conn
    }
    where
        bootStrap conn = liftIO $ forM_  bootstrapStmts (\q -> void $ execute_ conn q)
        bootstrapStmts = ["CREATE TABLE IF NOT EXISTS cache (key TEXT PRIMARY KEY, value TEXT)", "CREATE INDEX IF NOT EXISTS cache_key_index ON cache (key)"]
        get conn k = do
            res <- liftIO $ query conn "SELECT value FROM cache WHERE key = ?" (Only k)
            case res of
                [] -> pure Nothing
                [Only v] -> pure $ Just (deserialize v)
                _ -> error "Multiple values for the same key"

        put conn k v = liftIO $ void $ execute conn "INSERT INTO cache (key, value) VALUES (?, ?)" (k, serialize v)

        delete conn k = liftIO $ void $ execute conn "DELETE FROM cache WHERE key = ?" (Only k)

        keys conn = liftIO ( query_ conn "SELECT key FROM cache" :: IO [Only T.Text] ) >>= pure . map (\(Only k) -> k)

        clear conn = liftIO $ void $ execute_ conn "DELETE FROM cache"

        stats conn = liftIO $ do
            res <- query_ conn "SELECT COUNT(*) FROM cache"
            case res of
                [Only (n :: Int)] -> pure $ CacheStats n 0 n
                _ -> error "Multiple values for the same key"

