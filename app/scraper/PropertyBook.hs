{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

module Scraper.PropertyBook(
  module Scraper.Scraper,
  newPropertyBookScraper
  ) where

import Control.Applicative
import Data.Char (isAlphaNum, isPunctuation)
import qualified Data.Map as M
import Data.Text (Text)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Set as S
import Model.PropertyListing
import Model.Resource
import Scraper.Scraper
import Text.HTML.Scalpel hiding (URL, html)

newPropertyBookScraper :: (Monad m, Monad n) => m (WebScraper n PropertyListing)
newPropertyBookScraper = do
  pure $ WebScraper {..}
  where
    _scraper :: Scraper Text PropertyListing
    _scraper = propertyBookDetailsPageScraper

    runScraper res = case res of
      Html r -> case scrapeStringLike (html r) _scraper of
        Just x -> pure $ Right x
        Nothing -> pure $  Left $ ScrapeError "Failed to scrape"
      _ -> pure $ Left $ ScrapeError "Invalid resource type"

    extractLinks html = S.fromList $ fromMaybe [] (scrapeStringLike html extractLinks')

    extractLinks' :: Scraper Text [URL]
    extractLinks' = do
      listingLinks <- extractListingsLinks <|> pure []
      slideShowLinks <- extractSlideShowLinks <|> pure []
      if null listingLinks && null slideShowLinks then
        extractAllLinks' 
      else
        pure (listingLinks <> slideShowLinks)
  

extractListingsLinks :: Scraper Text [URL]
extractListingsLinks = do
   chroot ("div" @: [hasClass "propertyListings"]) extractAllLinks'

extractSlideShowLinks :: Scraper Text [URL]
extractSlideShowLinks = do
  chroot ("div" @: [hasClass "property-slideshows"]) extractAllLinks'

propertyBookDetailsPageScraper :: Scraper Text PropertyListing
propertyBookDetailsPageScraper  =
  chroot ("div" @: [hasClass "site-content"]) $ do
    (propertyListingPrice, propertyListingTitle, propertyListingId) <- propertyTitleExtractor 
    propertyPictures <- listingPicturesExtractor 
    propertyListingDate <- listingDateExtractor 
    propertyListingUrl <- pure "" 
    propertyListingAgent <- reAgentExtractor 
    propertyListingArea <- propertyAreaExtractor 
    propertyListingDescription <- propertyDescriptionExtractor 
    propertyListingFeatures <- propertyFeatures 
    (propertyListingStatus, propertyLocation)  <- propertyLocationAndStatusScraper 
    pure PropertyListing {..}

propertyLocationAndStatusScraper :: Scraper Text (ListingStatus, Location)
propertyLocationAndStatusScraper = do
  chroot ("div" @: [hasClass "single-listing-top"] // ("div" @: [hasClass "breadcrumb"])) $
    inSerial $ do
      status <- seekNext $ text "a"
      locationProvince <- seekNext $ text "a"
      locationCity <- seekNext $ text "a"
      locationRegion <- seekNext $ text "a"
      locationSuburb <- seekNext $ text "a"
      let locationCountry = "Zimbabwe"
      pure (listingStatusFromText status, Location {..})


propertyTitleExtractor :: Scraper Text (Price, Text, ListingID)
propertyTitleExtractor = do
  chroot ("div" @: [hasClass "property-title"] // "div") $
    inSerial $ do
      (price, listingId) <- seekNext $
        chroot "div" $
          inSerial $ do
            p <- seekNext $ text ("h4" @: [hasClass "property-price"])
            l <- seekNext $ text "span"
            pure (p, l)
      title <- seekNext $ text ("div" // "div" // "div" // "h1")
      pure (toPrice price, T.strip title, T.strip listingId)

propertyDescriptionExtractor :: Scraper Text Text
propertyDescriptionExtractor =
  chroot ("div" @: [hasClass "property-description"]) $ do
    header <- text "h6" <|> (pure "")
    paragraph <- text "p" <|> (pure header)
    pure $ T.strip header <> "\n" <> T.strip paragraph

propertyFeatures :: Scraper Text PropertyFeatures
propertyFeatures = do
  features <- chroots ("div" @: [hasClass "property-features"]) $ do
    chroots ("div" @: [hasClass "feature"]) $ featureExtractor <|> featureExtractor'
  pure $ M.fromList $ concat features

listingDateExtractor :: Scraper Text Text --TODO: change to UTCTime
listingDateExtractor = do
  date <- text ("div" @: [hasClass "listed-date"])
  pure $ T.strip $ T.replace "Listed On:" "" date

propertyAreaExtractor :: Scraper Text PropertyArea
propertyAreaExtractor = do
  chroot ("div" @: [hasClass "single-icons"] // "ul" @: [hasClass "list-inline"] // "li") $ do
    _ <- text ("li" @: [hasClass "list-inline-item"] // "span" @: [hasClass "customIcon-property-size"]) <|> pure ""
    area <- text ("li" @: [hasClass "list-inline-item"]) <|> pure "0"
    pure $ extractArea $ T.strip area

featureExtractor :: Scraper Text (PropertyFeature, Text)
featureExtractor =
  inSerial $ do
    value <- seekNext $ text ("span" @: [hasClass "value"])
    feature <- seekNext $ text ("span" @: [notP $ hasClass "value"])
    pure (T.strip feature, T.strip value)

featureExtractor' :: Scraper Text (PropertyFeature, Text)
featureExtractor' =
  inSerial $ do
    feature <- seekNext $ text ("span" @: [notP $ hasClass "check"])
    pure $ (T.strip feature, "")

reAgentExtractor :: Scraper Text RealEstateAgent
reAgentExtractor = chroot ("div" @: [hasClass "block"]) $ do
  agentName' <- text ("strong" @: [hasClass "agentName"]) <|> (pure "")
  agentCompany' <- text ("a" // "div" // "h5") <|> (pure "")
  agentTelephone' <- text ("span" @: [hasClass "agentNumber"]) <|> (pure "")
  agentLogo <- attr "src" ("a" // "div" // "img") <|> (pure "")
  let agentName = T.filter (not . isPunctuation) $ T.strip agentName'
      agentCompany = T.strip agentCompany'
      agentTelephone = T.filter isAlphaNum $ T.strip agentTelephone'
  pure RealEstateAgent {..}

listingPicturesExtractor :: Scraper Text [ListingPicture]
listingPicturesExtractor = chroot ("div" @: [hasClass "propertySlideshow"] // "ul") $ do
  chroots "li" $ do
    listingThumbnail <- attr "data-thumb" "li"
    listingFull <- attr "data-src" "li"
    pure ListingPicture {..}
