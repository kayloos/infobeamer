{-# LANGUAGE OverloadedStrings #-}
module Infobeamer.User (userActions) where

import Web.Spock
import Data.Text hiding (map)
import qualified Data.Text.Lazy as L
import qualified Database.RethinkDB as R
import Control.Monad
import Control.Monad.IO.Class

import Infobeamer.Base
import Infobeamer.Validation

userActions :: SpockM R.RethinkDBHandle User b ()
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
    {- checkUser handle pw -}
    signUserIn handle
    redirect "/"
  
  post "/create/user" $ do
    [handle, pw, pw'] <- mapM param [ "user[handle]"
                                    , "user[password]"
                                    , "user[password_confirmation]"
                                    ]

    errs <- runQuery $ performValidations [validateHandle, validatePassword]
                                          ["handle", "password"]
                                          [handle, pw]

-- FIXME: password matches confirmation unchecked.
    if Prelude.null errs
       then do
         runQuery $ run' $ R.table "users" R.#
           R.insert (R.obj [ "handle" R.:= R.str handle
                           , "pw"     R.:= R.str pw
                           ])
         signUserIn handle
       else raise $ "Validation error " `L.append` L.pack (show errs)
    redirect "/"

type User = String
type UserSession = String
type Password = String

-- Check if user is authed, otherwise send him off to default page
checkUser :: User -> Password -> SpockAction conn UserSession st ()
checkUser usr = undefined -- do
  {- user <- runQuery $ run $ R.table "users" -- rewrite to fetch user object (get by handle) -}

signUserIn :: User -> SpockAction conn UserSession st ()
signUserIn usr = authedUser usr (\s -> s)

performValidations :: [Validation R.RethinkDBHandle ()] -> [Column] -> [Subject] ->
                      R.RethinkDBHandle -> IO [Message]
performValidations [] [] [] _ = return []
performValidations (v:vs) (c:cs) (s:ss) h = do
  liftM2 (++) (performValidation v c s h) (performValidations vs cs ss h)

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

{- isBlank :: Text -> Bool -}
{- isBlank txt = Data.Text.null txt -}

{- Things to do: -}
{- - encrypt password using a proper technique -}
{- - user handles must be unique (validations) -}

{- Validations: -}
{- - Run each validation -}
{- - If validation fails, add error message -}
