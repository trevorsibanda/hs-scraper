{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Scraper.Scraper where

import Data.Text (Text)
import Model
import Text.HTML.Scalpel hiding (URL)
import qualified Data.Set as S

data ScrapeError = ScrapeError Text deriving (Show, Eq)

data WebScraper m a = WebScraper
  { _scraper :: Scraper Text a,
    runScraper :: Resource -> m (Either ScrapeError a),
    extractLinks :: Text -> S.Set URL
  }
