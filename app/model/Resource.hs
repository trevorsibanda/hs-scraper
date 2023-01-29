{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Model.Resource where


import Model.Serde
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Set as S
import Text.HTML.Scalpel hiding (URL)
import Network.URI (parseURI, uriToString, uriScheme, uriPath, uriAuthority, uriRegName)


-- | URL is a data type that represents a URL
type URL = Text


-- | Resource is a data type that represents a resource that can be
-- | scraped from a website.
data Resource = Html HtmlPage | Image URL | CSS URL | JS URL | Other URL deriving (Show, Eq)

instance Serde HtmlPage where
    serialize HtmlPage {..} = T.concat [url, ";", html]

    deserialize t = HtmlPage {..}
        where
            (url, html) = T.breakOn ";" t

-- | newHTMLPage creates a new HTML page from a URL and the HTML content
newHTMLPage :: URL -> Text -> HtmlPage
newHTMLPage url html = let 
    uri = fromMaybe (error "Invalid URL") (parseURI (T.unpack url))
    [scheme, path] = T.pack <$> ([uriScheme, uriPath] <*> [uri])
    host =  case (uriAuthority uri)  of 
      Just auth -> T.pack $ uriRegName auth
      Nothing -> ""
    [links, images, css, js, other, fonts] = [extractAllLinks, extractImages, extractCSS, extractJS, const S.empty, const S.empty ] <*> [html]
  in 
    HtmlPage {..}

data HtmlPage = HtmlPage
  { url :: URL,
    scheme :: Text,
    host :: Text,
    path :: Text,
    html :: Text,
    links :: S.Set URL,
    images :: S.Set URL,
    css :: S.Set URL,
    js :: S.Set URL,
    other :: S.Set URL,
    fonts :: S.Set URL
  }
  deriving (Show, Eq)



isExternal :: URL -> URL -> Bool
isExternal url1 url2 = case (parseURI (T.unpack url1), parseURI (T.unpack url2)) of
  (Just uri1, Just uri2) -> uriAuthority uri1 /= uriAuthority uri2
  _ -> False

parseURLAuthority url = case parseURI (T.unpack url) of
  Just uri -> uriAuthority uri
  Nothing -> Nothing

extractAllLinks :: Text -> S.Set URL
extractAllLinks html = S.fromList $ fromMaybe [] (scrapeStringLike   html extractAllLinks')

normalizeLink :: URL -> Maybe URL
normalizeLink link = case parseURI (T.unpack link) of
  Just uri -> if(uriScheme uri == "http:" || uriScheme uri == "https:") then
      Just $ T.pack $ uriToString id uri ""
    else Nothing
  Nothing -> Nothing

-- Extract links from a page
extractAllLinks' :: Scraper Text [URL]
extractAllLinks' = do
  links <- attrs "href"  "a" 
  pure $ filter (isJust . normalizeLink) links
    

-- Extract images from a page
extractImages' :: Scraper Text [URL]
extractImages' = do
  images <- attrs "src" $ "img"
  --TODO: extract from <picture> tag
  pure images

extractImages :: Text -> S.Set URL
extractImages html = S.fromList $ fromMaybe [] (scrapeStringLike  html extractImages')

-- Extract CSS from a page
extractCSS' :: Scraper Text [URL]
extractCSS' = do
  css <- attrs "href" $ "link" @: ["rel" @= "stylesheet"]
  pure $ css 

extractCSS :: Text -> S.Set URL
extractCSS html = S.fromList $ fromMaybe [] (scrapeStringLike  html extractCSS')

-- Exxtract javascript from a page
extractJS' :: Scraper Text [URL]
extractJS' = do
  js <- attrs "src" $ "script"
  pure $ filter (not. T.null) js

extractJS :: Text -> S.Set URL
extractJS html = S.fromList $ fromMaybe [] (scrapeStringLike  html extractJS') 
