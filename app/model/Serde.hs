{-# LANGUAGE OverloadedStrings #-}
{-# Language RecordWildCards #-}
module Model.Serde where

import Data.Text (Text)
import qualified Data.Text as T

class Serde a where
    serialize :: a -> Text
    deserialize :: Text -> a


class HasLookupKey a where
  lookupKey :: a -> Text
