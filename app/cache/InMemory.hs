module Cache.InMemory (module Cache.Cache, newInMemoryCache, withInMemoryCache) where

import Control.Monad.IO.Class
import UnliftIO
import UnliftIO.Concurrent
import Cache.Cache
import qualified UnliftIO.STM  as STM

import qualified Data.Map as M
import qualified Data.Text as T
import Model.Serde

newInMemoryCache ::(MonadUnliftIO m, MonadUnliftIO n, Show a, Serde a) => n (Cache m a)
newInMemoryCache =  do
    cache <- STM.atomically $ STM.newTVar m
    pure Cache{
        cacheType = InMemoryCache,
        get = get cache,
        put = put cache,
        delete = delete cache,
        keys = keys cache,
        clear = clear cache,
        stats = stats cache
    }
    where
        m :: M.Map T.Text a
        m = M.empty

        get cache k = do
            st <- STM.atomically $ STM.readTVar cache
            case M.lookup k st of
                Nothing -> pure Nothing
                Just v -> pure $ Just (deserialize v)

        put cache k v = STM.atomically $ STM.readTVar cache >>= (\st -> STM.writeTVar cache $ M.insert k (serialize v) st)

        delete cache k = STM.atomically $ STM.readTVar cache >>= (\st -> STM.writeTVar cache $ M.delete k st)

        keys cache =  STM.atomically $ STM.readTVar cache >>= pure . M.keys

        clear cache = STM.atomically $ STM.writeTVar cache M.empty

        stats cache = STM.atomically $ STM.readTVar cache >>= (\st -> pure $ CacheStats (M.size st) 0 (M.size st))


withInMemoryCache :: (MonadUnliftIO m, Show a, Serde a) => (Cache m a -> m b) -> m b
withInMemoryCache = withCache' newInMemoryCache
