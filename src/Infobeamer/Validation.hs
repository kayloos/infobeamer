module Infobeamer.Validation where

import Data.Maybe (catMaybes)
import Control.Monad.IO.Class
import Text.Regex

type Subject = String
type Message = String
type Column  = String
type UniqueQuery dbHandle = Column -> Subject -> dbHandle -> IO Bool

newtype Book dbHandle = Book (Column, Subject, dbHandle, UniqueQuery dbHandle)
type ValidationErrors = [Maybe Message]

newtype Validation a b = Validation
                           { unValidation :: Book a -> IO (b, ValidationErrors) }

runValidation :: Validation a b -> Book a -> IO [Message]
runValidation v s = do
  (_, valErrs) <- unValidation v s
  return $ catMaybes valErrs

instance Monad (Validation a) where
  return x = Validation $ \_ -> return (x, [Nothing])
  (Validation v) >>= f = Validation $ \subject -> do
    (y, errs) <- v subject
    (z, errs') <- unValidation (f y) subject
    return (z, errs ++ errs')

instance MonadIO (Validation a) where
  liftIO a = Validation $ \_ -> do
    a' <- a
    return (a', [Nothing])

validationError :: Message -> Validation a ()
validationError message = Validation $ \_ -> return ((), [Just message])

subject :: Validation a Subject
subject = Validation $ \(Book (_, subject, _, _)) -> return (subject, [Nothing])

handle :: Validation a a
handle = Validation $ \(Book (_, _, dbHandle, _)) ->
  return (dbHandle, [Nothing])

column :: Validation a Column
column = Validation $ \(Book (column, _, _, _)) -> return (column, [Nothing])

uniquenessQuery :: Validation a (UniqueQuery a)
uniquenessQuery = Validation $ \(Book (_, _, _, qry)) -> return (qry, [Nothing])

-- | Returns true if column is unique
validateUniqueness :: Validation a ()
validateUniqueness = do
  column    <- column
  subject   <- subject
  handle    <- handle
  queryFunc <- uniquenessQuery
  result    <- liftIO $ queryFunc column subject handle
  if result
     then return ()
     else validationError $ "Identifier `" ++ subject ++ "' is not unique"

validateLength :: (Int -> Int -> Bool) -> Int -> Validation a ()
validateLength comparisonFunc len = do
  subj <- subject
  if length subj `comparisonFunc` len
     then return ()
     else validationError $ "Length of `" ++ subj ++ "' not greater than `" 
                            ++ show len ++ "'"

validateMatch :: String -> String -> Validation a ()
validateMatch str str'
  | str == str' = return ()
  | otherwise   = validationError $ "Strings `" ++ str ++
                                    "' and `" ++ str' ++ "' do not match"

validateRegex :: String -> Validation a ()
validateRegex regexp = do
  subject' <- subject
  case matchRegex (mkRegex regexp) subject' of
    Nothing ->
      validationError $ "Identifier `" ++ subject' ++ 
                        "' did not match the regular expression `" ++ 
                        show regexp ++ "'"
    Just _ -> return ()

{-
What should be validated?
  - fields that are about to be saved to the database
    * strings

Which validations should we have
  - Empty, non-empty
  - Regexp
  - In a list, not in a list
  - length
  - numericality
  - uniqueness

Validations should be able to be combined like so:

validateTitle = do
  validatePresence
  validateLength LT 180
  validateUnique

if one or more validations fail, a list of error messages is returned.
it is enough that we can infer success by the fact that there is no error messages

-}
