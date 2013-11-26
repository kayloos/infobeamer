{-# LANGUAGE OverloadedStrings #-}
module Feed (feedActions) where

import Control.Monad (liftM)
import Control.Monad.IO.Class
import Data.AesonBson
import Data.Text hiding (map)
import qualified Data.Text.Lazy as L
import qualified Database.RethinkDB as R
import Web.Scotty

import Base

feedActions :: R.RethinkDBHandle -> ScottyM ()
feedActions h = do
  post "/feed" $ do
    feed <- param "feed"
    dict <- getRss feed
    json $ aesonify dict

  post "/feed/create" $ do
    link <- param "feed[link]"
    liftIO $ R.run' h $ R.table "feeds" R.# R.insert (R.obj ["link" R.:= R.str link])
    redirect "/"

  get "/feed/delete/:feedId" $ do
    ident <- liftM L.toStrict (param "feedId")
    poo   <- liftIO $ R.run' h (R.delete (R.get (R.expr ident) (R.table "feeds")))
    redirect "/"
