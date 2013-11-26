{-# LANGUAGE OverloadedStrings #-}
module User (userActions) where

import Web.Scotty
import Base
import Data.Text
import qualified Data.Text.Lazy as L
import qualified Database.RethinkDB as R

userActions :: R.RethinkDBHandle -> ScottyM ()
userActions h = do
  get "/users" $ do
    text "all users"

  get "/new/user" $ do
    content <- render' "user/new" []
    html content
  
  post "/create/user" $ do
    text "create user"

