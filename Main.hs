{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.IO.Class (liftIO)
import Data.AesonBson
import Data.Bson hiding ((:=))
import qualified Data.Aeson as A
import Data.Monoid (mconcat)
import Data.Text hiding (map)
import qualified Data.Text.Lazy as L
import qualified Database.RethinkDB as R

import Hemplate.Base
import Web.Scotty
import Web.Scotty.Session

import Base
import ErrorMessages
import Feed
import User


main :: IO ()
main = scotty 3000 $ do
  h <- openConnection
  sesh <- createSessionManager
  get "/" $ do
    rss <- liftIO $ R.run h (R.table "feeds")
    str <- render' "landing" ["rss" =: Prelude.map bsonifyValue rss]
    html str

  get "/js/:name" $ do
    fileName <- param "name"
    setHeader "Content-type" "application/javascript"
    file $ "front/js/" ++ fileName

  get "/css/:name" $ do
    fileName <- param "name"
    setHeader "Content-type" "text/css"
    file $ "front/css/" ++ fileName

  feedArg h [feedActions, userActions]

  where
    feedArg :: Monad m => a -> [a -> m b] -> m ()
    feedArg a [] = return ()
    feedArg a (x:xs) = x a >> feedArg a xs
    bsonifyValue' (R.JSON val) = bsonifyValue val
