{-# LANGUAGE OverloadedStrings #-}
module Infobeamer.Queries where

import Database.RethinkDB
import Data.Text
import Data.Aeson (Value)
import qualified Data.Time as DT

r :: (Expr query, Result r) => query -> RethinkDBHandle -> IO r
r = flip Database.RethinkDB.run

insertFeed :: Text -> RethinkDBHandle -> IO [Value]
insertFeed link = 
  r $ insert (obj ["link" := expr link]) feeds

deleteFeed :: Text -> RethinkDBHandle -> IO [String]
deleteFeed ident =
  r $ delete $ get (expr ident) feeds

feeds :: Table
feeds = table "feeds"

insertUser :: Text -> Text -> RethinkDBHandle -> IO [Value]
insertUser handle pw =
  r $ insert (obj [ "handle" := handle
                  , "pw"     := pw
                  ]) users

insertSession :: DT.UTCTime -> Text -> RethinkDBHandle -> IO [Value]
insertSession expiresAt userId =
  r $ insert (obj [ "expiresAt" := expiresAt
                  , "userId"    := userId]) sessions

getUserPwByHandle :: Text -> RethinkDBHandle -> IO (Maybe Text)
getUserPwByHandle userHandle =
  r $ (getAll "handle" [userHandle] users) ! "pw"

getUserIdByHandle :: Text -> RethinkDBHandle -> IO [Text]
getUserIdByHandle userHandle =
  r $ (getAll "handle" [userHandle] users) ! "id"


sessions :: Table
sessions = table "sessions"
users :: Table
users = table "users"
