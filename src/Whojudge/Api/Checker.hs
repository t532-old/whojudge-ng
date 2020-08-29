{-# LANGUAGE Rank2Types #-}

module Whojudge.Api.Checker where

import qualified Data.ByteString.Lazy as BS
import           Data.Function
import qualified Data.Text as T
import qualified Database.Persist as DB
import           Servant
import qualified Whojudge.Api.Error as Err
import qualified Whojudge.Config as Conf
import           Whojudge.Database.Schema
import qualified Whojudge.Database.Util as DB

auth :: DB.Handle -> Maybe T.Text -> Handler (UserId, User)
auth perform (Just token) =
  userAuth
    >>= (& nothing Err.notLoggedIn)
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

validUsername :: T.Text -> Handler ()
validUsername username =
  Conf.usernamePredicate username
    & false err400 {errBody = Conf.usernameTip}
    
validPassword :: T.Text -> Handler ()
validPassword password =
  Conf.passwordPredicate password
    & false err400 {errBody = Conf.passwordTip}

nothing :: ServerError -> Maybe a -> Handler a
nothing _ (Just x) = pure x
nothing e Nothing = throwError e

left :: ServerError -> Either a b -> Handler b
left _ (Right y) = pure y
left e (Left _) = throwError e

false :: ServerError -> Bool -> Handler ()
false _ True = pure ()
false e False = throwError e
