module Main where

import UnliftIO
import Control.Monad.IO.Class
import Data.Text (Text)
import DataLake.InMemory
import DataLake.Postgres
import Model
import Scraper.PropertyBook
import Crawler.Simple

newPropertyBookInMemoryCrawler :: (MonadUnliftIO m, MonadFail m) => m (Crawler m PropertyListing)
newPropertyBookInMemoryCrawler = do
  _scraper <- newPropertyBookScraper
  newSimpleCrawler _scraper defaultConfig

newPropertyBookPostgresCrawler :: (MonadUnliftIO m, MonadFail m) => Text -> m (Crawler m PropertyListing)
newPropertyBookPostgresCrawler connString = do
  _scraper <- newPropertyBookScraper
  _datalake <- newPostgresDataLake connString
  newPersistentCrawler _scraper _datalake defaultConfig connString

main :: IO ()
main = putStrLn "Hello, Haskell!"
