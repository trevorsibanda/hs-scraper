{-# LANGUAGE OverloadedStrings  #-}
module Model
  (
    module Model.PropertyListing
    , module Model.Resource
    , module Model.Serde
  )
where

import Model.PropertyListing
import Model.Resource
import Model.Serde

import Data.Text (Text)

modelVersion :: Text
modelVersion = "0.0.1"