{-# LANGUAGE OverloadedStrings #-}

module Infobeamer.Base where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Error.Class (MonadError)
import Control.Monad.Trans.Either (EitherT, runEitherT, left)
import Control.Monad (liftM)
import Data.AesonBson
import Data.Bson hiding ((:=))
import Data.Maybe (fromJust)
import Data.Monoid (mconcat)
import Data.Text hiding (map)
import qualified Data.Text.Lazy as L

import Network.Curl
import qualified Database.RethinkDB as R
import Hemplate.Base
import Text.XML.Light.Input
import Text.RSS.Import
import Text.RSS.Syntax

import Infobeamer.ErrorMessages

type RSSRenderer a = EitherT String IO a

render' ::  MonadIO m => FilePath -> Document -> m L.Text
render' target dict = do
  res <- liftIO $ renderHemplate "front/application.html" ("front/" ++ target ++ ".hemptml") dict
  return $ L.fromStrict res

-- Use hemplate to render RSS items
renderRSSItem :: RSSItem -> Document
renderRSSItem item = [ "title" =: title
                     , "link"  =: link
                     , "desc"  =: desc
                     ]
  where
    g f = pack $ fromJust (f item)
    [title, link, desc] = map g [rssItemTitle, rssItemLink, rssItemDescription]

-- Use hemplate to render RSS feeds
rssDict :: String -> RSSRenderer Document
rssDict url = do
  xml <- do
    raw <- fetchFeed url
    maybe (left rawToXmlFail) return (parseXMLDoc raw)
  rss <- maybe (left xmlToRssFail) return (elementToRSS xml)
  return ["rss" =: map renderRSSItem (rssItems $ rssChannel rss)]

fetchFeed :: String -> RSSRenderer String
fetchFeed target = do
  response <- liftIO $ curlGetString_ target []
  case response of
    (CurlOK, str) -> return str
    (code, str)   -> left $ curlFail $ show code

getRss :: MonadIO m => String -> m Document
getRss str = do
  foo <- liftIO $ runEitherT $ rssDict str
  case foo of
    Left e    -> error $ "Could not interpret feed:\n" ++ e
    Right doc -> return doc

openConnection :: IO R.RethinkDBHandle
openConnection = liftM (flip R.use (R.db "infobeamer"))
                       (R.connect "localhost" 28015 Nothing)

run a b = R.run b a
run' a b = R.run' b a
