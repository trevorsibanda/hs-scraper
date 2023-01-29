{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Model.PropertyListing where

import Data.Char (isDigit, isAlpha)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map as M
import Model.Resource (URL)
import Model.Serde

import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToRow

-- | A property listing contains all the information about a property
-- | as listed on the website.
-- | This is the data that is scraped from the website.

data PropertyListing = PropertyListing
  { propertyListingUrl :: URL,
    propertyListingId :: PropertyId,
    propertyListingDate :: Text, -- TODO: Change to UTCTime
    propertyPictures :: [ListingPicture],
    propertyListingTitle :: Text,
    propertyListingStatus :: ListingStatus,
    propertyLocation :: Location,
    propertyListingPrice :: Price,
    propertyListingArea :: PropertyArea,
    propertyListingFeatures :: PropertyFeatures,
    propertyListingDescription :: Text,
    propertyListingAgent :: RealEstateAgent
  }
  deriving (Eq)


instance FromRow PropertyListing where
  fromRow = undefined

instance ToRow PropertyListing where
  toRow = undefined

instance Show PropertyListing where
  show PropertyListing{..} = 
    T.unpack $ propertyListingId <> ": " <> propertyListingTitle <> " by " <> (agentName propertyListingAgent) <> " for " <> (T.pack $ show propertyListingPrice)

instance HasLookupKey PropertyListing where
  lookupKey = propertyListingId

instance Serde PropertyListing where
    serialize p = T.pack $ show p 
    deserialize t = undefined


-- | Picture of the property
data ListingPicture = ListingPicture
  { listingThumbnail :: URL,
    listingFull :: URL
  }
  deriving (Show, Eq)


-- | A property feature,  i.e bedrooms, swimming pool, etc.
type PropertyFeature = Text

-- | Property features,  i.e bedrooms, swimming pool, etc.
type PropertyFeatures = M.Map PropertyFeature Text

-- | The property ID as listed on the website
type PropertyId = Text

-- | Listing ID
type ListingID = Text


type Suburb = Text

type Province = Text

type Country = Text

type City = Text

-- | Location of the property
data Location = Location
  { locationSuburb :: Suburb,
    locationProvince :: Province,
    locationCountry :: Country,
    locationRegion :: Text,
    locationCity :: City
  }
  deriving (Show, Eq)

-- | Area of the property
data PropertyArea = SquareMeters Int | SquareKilometers Int | Hectares Int | Acres Int | OtherArea Text | NoArea deriving (Show, Eq)

-- | The real estate agent that is listing the property
data RealEstateAgent = RealEstateAgent
  { agentName :: Text,
    agentCompany :: Text,
    agentTelephone :: Text,
    agentLogo :: URL
  }
  deriving (Show, Eq)

-- | Types of property
data PropertyType = House | Apartment | Townhouse | Unit | Land | OtherPropertyType Text deriving (Show, Eq)

-- | Unused: population density of area where the property is located
data LocationDensity = Low | Medium | High | CBD | Rural | Outskirts | Unclassified deriving (Show, Eq)

-- | Status of the property listing
data ListingStatus = ForSale | ForRent | Sold | Leased | UnderOffer | OnHold | Auction | SoleMandate | ReducedPrice | NewDevelopment | OtherStatus Text deriving (Show, Eq)

listingStatusFromText :: Text -> ListingStatus
listingStatusFromText s = case T.strip $ T.toLower s of
  "property for sale" -> ForSale
  "property to rent" -> ForRent
  _ -> OtherStatus s


extractArea :: Text -> PropertyArea
extractArea a =
  if (T.length num' == 0)
    then OtherArea a
    else case suffix of
      "m2" -> SquareMeters num
      "ha" -> Hectares num
      "ac" -> Acres num
      "km2" -> SquareKilometers num
      _ -> OtherArea a
  where
    num' = T.takeWhile isDigit  (T.strip a)
    num = read (T.unpack num') :: Int
    suffix = T.toLower $ T.dropWhile (not . isAlpha) (T.replace "," "" a)


-- | Price of the property
data Price = USD Double | PriceOnApplication | None deriving (Show, Eq)

removeNonNumericChar :: Text -> Text
removeNonNumericChar = T.filter (`elem` ['0' .. '9'])

toPrice :: Text -> Price
toPrice p = case T.strip p of
  "POA" -> PriceOnApplication
  "None" -> None
  _ -> USD $ read $ T.unpack $ removeNonNumericChar p

