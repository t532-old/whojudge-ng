{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE Rank2Types #-}
module Whojudge.Api.Util where

import qualified Data.ByteString.Lazy as BS
import           Data.Function
import           Data.Maybe
import qualified Data.Text as T
import qualified Database.Persist as DB
import           Servant
import           Whojudge.Database.Schema
import           Whojudge.Database.Util

checkAuth :: Database -> Maybe T.Text -> Handler (UserId, User)
checkAuth perform (Just token) =
  userAuth
    >>= (& ifNothing err401 {errBody = "You are not logged in."})
  where
    userAuth = perform $ do
      authEnt <- DB.getBy $ AuthToken token
      case authEnt of
        Nothing -> pure Nothing
        Just (DB.Entity authId auth) -> do
          userEnt <- DB.get (authorizationUser auth)
          case userEnt of
            Nothing -> do
              DB.delete authId
              pure Nothing
            Just user -> pure $ Just (authorizationUser auth, user)

ifNothing :: ServerError -> Maybe a -> Handler a
ifNothing _ (Just x) = pure x
ifNothing e Nothing = throwError e

ifLeft :: ServerError -> Either a b -> Handler b
ifLeft _ (Right y) = pure y
ifLeft e (Left _) = throwError e

ifNot :: ServerError -> Bool -> Handler ()
ifNot _ True = pure ()
ifNot e False = throwError e
