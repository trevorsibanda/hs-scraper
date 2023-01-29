{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Cache.Cache where

import UnliftIO
import Data.Text (Text)
import qualified Data.Text as T
import Model.Serde (Serde)

-- | A cache type
data CacheType = NoCache | InMemoryCache | RedisCache Text | PostgresCache Text  | FileCache Text deriving (Show, Eq)

-- | Cache statistics
data CacheStats = CacheStats {
    csHits :: Int,
    csMisses :: Int,
    csSize :: Int
}

-- | A cache is a key-value store that can be used to store and retrieve data.
data Cache m a = Cache {
  cacheType :: CacheType,
  get :: T.Text -> m (Maybe a),
  put :: T.Text -> a -> m (),
  delete :: T.Text -> m (),
  keys :: m [T.Text],
  clear :: m (),
  stats :: m CacheStats
}


withCache' :: (MonadUnliftIO m, Show a, Serde a) => (forall n. MonadUnliftIO n => n (Cache m a)) -> (Cache m a -> m b) -> m b
withCache' newCache f = do
    cache <- newCache
    f cache

withCache :: (MonadUnliftIO m, Show a, Serde a) => Cache m a -> (Cache m a -> m b) -> m b
withCache cache f = f cache
