{-# LANGUAGE OverloadedStrings #-}
module Infobeamer.User where

import Web.Spock
import Web.Spock.Auth
import Web.Scotty.Trans (ActionT)
import Data.Text hiding (map)
import qualified Data.Text.Lazy as L
import qualified Database.RethinkDB as R
import Control.Monad
import Control.Monad.IO.Class
import Data.Time

import Infobeamer.Base
import Infobeamer.Validation
import Infobeamer.Queries

userActions :: SpockM R.RethinkDBHandle (VisitorSession () Text) b ()
userActions = do
  get "/users" $ do
    text "Show All Users"

  get "/new/user" $ do
    render' "user/new" [] >>= html

  get "/new/session" $ do
    render' "user/new_session" [] >>= html

  post "/create/session" $ do
    [handle, pw] <- mapM param [ "user[handle]"
                               , "user[password]"
                               ]
    signUserIn handle pw
    redirect "/"
  
  post "/create/user" $ do
    [handle, pw, pw'] <- mapM param [ "user[handle]"
                                    , "user[password]"
                                    , "user[password_confirmation]"
                                    ]

    errs <- runQuery $ performValidations [validateHandle, validatePassword]
                                          ["handle", "password"]
                                          [handle, pw]

    signUserIn handle pw

  {- -- FIXME: password matches confirmation unchecked. -}
    if Prelude.null errs
        then runQuery $ insertUser handle pw
        else raise $ "Validation error " `L.append` L.pack (show errs)
    redirect "/"

getUserId :: Text -> Text -> R.RethinkDBHandle -> IO (Either Message Text) 
getUserId userHandle password dbHandle = do
  pw <- getUserPwByHandle userHandle dbHandle
  case pw of
    Just str ->
      if str == password
         then getUserIdByHandle userHandle dbHandle >>= return . Right . Prelude.head 
         else return $ Left "Password did not match"
    Nothing -> return $ Left "Could not find user with handle"

signUserIn handle pw = do
    userId <- runQuery $ getUserId handle pw
    case userId of
         Left msg -> error msg
         Right userId' -> do
           (runQuery $ createSession userId')
           markAsLoggedIn userId'

createSession userId dbHandle = do
  now <- getCurrentTime
  insertSession (year `addUTCTime` now) userId dbHandle
  where year = 60 * 60 * 24 * 365


performValidations :: [Validation R.RethinkDBHandle ()] -> [Text] -> [Text] ->
                      R.RethinkDBHandle -> IO [Message]
performValidations [] [] [] _ = return []
performValidations (v:vs) (c:cs) (s:ss) h = do
  liftM2 (++) (performValidation v c' s' h) (performValidations vs cs ss h)
  where [c', s'] = map unpack [c,s]

performValidation :: Validation R.RethinkDBHandle () -> Column -> Subject ->
                     R.RethinkDBHandle -> IO [Message] 
performValidation v c s h = runValidation v (Book (c, s, h, uniqQuery))

validatePassword :: Validation R.RethinkDBHandle ()
validatePassword = do
  validateLength (<) 45
  validateLength (>) 3

validateHandle :: Validation R.RethinkDBHandle ()
validateHandle = validateUniqueness

uniqQuery :: Column -> Subject -> R.RethinkDBHandle -> IO Bool
uniqQuery col sub han = do
  q <- qry
  return $ if Prelude.null q
             then True
             else False
  where qry = R.run han $ R.getAll (pack col) [R.expr (pack sub)] (R.table "users") ::
                IO [R.JSON]
