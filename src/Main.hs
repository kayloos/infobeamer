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
import Web.Spock

import Infobeamer.Base
import Infobeamer.ErrorMessages
import Infobeamer.Feed
import Infobeamer.User

storage = PCConn $ ConnBuilder {
    cb_createConn = openConnection,
    cb_destroyConn  = R.close,
    cb_poolConfiguration = PoolCfg { pc_stripes = 1, pc_resPerStripe = 1, pc_keepOpenTime = 3000 }
  }

main :: IO ()
main = spock 3000 storage [] $ do
  get "/" $ do
    rss <- runQuery $ run $ R.table "feeds"
    str <- render' "landing" ["rss" =: Prelude.map bsonifyValue rss]
    html str

  get "/js/:name" $ do
    fileName <- param "name"
    setHeader "Content-type" "application/javascript"
    file $ "../front/js/" ++ fileName

  get "/css/:name" $ do
    fileName <- param "name"
    setHeader "Content-type" "text/css"
    file $ "../front/css/" ++ fileName

  feedActions
  userActions
