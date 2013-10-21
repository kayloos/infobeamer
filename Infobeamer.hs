{-# LANGUAGE OverloadedStrings #-}

module Infobeamer where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad (liftM)
import Data.Bson
import Data.Maybe
import Data.Monoid (mconcat)
import Data.Text.Lazy

import Hemplate.Base
import Text.XML.Light.Input
import Text.RSS.Import
import Text.RSS.Syntax
import Web.Scotty

render' :: MonadIO m => FilePath -> Document -> m Text
render' beef sauce = do
  res <- liftIO $ renderHemplate "templates/application.html" beef sauce
  return $ pack res

-- Use hemplate to render RSS items
renderRSSItem :: RSSItem -> String
renderRSSItem item = fromJust (rssItemTitle item)

-- Use hemplate to render RSS feeds
{- renderRSS :: RSS -> Text -}
renderRSS rss = rss >>= return . (Prelude.map renderRSSItem) . rssItems . rssChannel

main :: IO ()
main =
  scotty 3000 $ do
    get "/" $ do
      -- feed main page
      str <- render' "templates/infobeamer.html" []

      xml <- do
        parseResult <- liftM parseXMLDoc $ liftIO $ readFile "ycomb.xml"
        case parseResult of
          Nothing  -> Web.Scotty.raise "Nyukkah"
          Just res -> return res

      rss <- case elementToRSS xml of
                Nothing -> raise "Nyukkah2"
                Just rss -> return rss
      html $ mconcat [str]

test :: IO RSS
test = do
  xml <- do
    parseResult <- liftM parseXMLDoc $ liftIO $ readFile "ycomb.xml"
    case parseResult of
      Nothing  -> fail "Nyukkah"
      Just res -> return res
  rss <- case elementToRSS xml of
            Nothing -> fail "Nyukkah2"
            Just rss -> return rss
  return rss


