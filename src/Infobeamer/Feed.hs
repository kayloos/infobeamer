{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction #-}
module Infobeamer.Feed (feedActions) where

import Control.Monad (liftM)
import Control.Monad.IO.Class
import Data.AesonBson
import Data.Text hiding (map)
import qualified Data.Text.Lazy as L
import qualified Database.RethinkDB as R
import Web.Spock
import Web.Scotty.Trans

import Infobeamer.Base
import Infobeamer.Queries

feedActions :: SpockM R.RethinkDBHandle a b ()
feedActions = do
  post "/feed" $ do
    feed <- param "feed"
    dict <- getRss feed
    json $ aesonify dict

  post "/feed/create" $ do
    link <- param "feed[link]"
    runQuery $ insertFeed link
    redirect "/"

  get "/feed/delete/:feedId" $ do
    ident <- liftM L.toStrict (param "feedId")
    runQuery $ deleteFeed ident
    redirect "/"
