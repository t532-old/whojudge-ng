{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}

module Whojudge.Api.User.Schema where

import           Control.Monad
import           Control.Monad.IO.Class
import qualified Crypto.KDF.BCrypt as BCrypt
import qualified Data.Aeson as JSON
import           Data.Function
import           Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Time
import qualified Database.Persist as DB
import           GHC.Generics
import           Servant
import qualified Whojudge.Api.Checker as Check
import qualified Whojudge.Api.Types as Api
import qualified Whojudge.Config as Conf
import           Whojudge.Database.Schema
import           Whojudge.Database.Util as DB

type EndpointName = "users"

type Entry = User

type Point =
  Api.Authorized :>
    Api.Point EndpointName
      Creation
      Retrieve
      Update
      Criteria

data Creation = Creation
  { username :: T.Text
  , password :: T.Text
  } deriving (Show, Generic, JSON.FromJSON, JSON.ToJSON)

toEntry :: Creation -> Handler Entry
toEntry Creation{..} = do
  Check.validUsername username
  Check.validPassword password
  passwordHash <- liftIO $ BCrypt.hashPassword 10 (T.encodeUtf8 password)
  current <- liftIO getCurrentTime
  pure User
    { userUsername = username
    , userPasswordHash = T.decodeUtf8 passwordHash
    , userDescription = ""
    , userIsAdmin = False
    , userCreatedAt = current
    , userModifiedAt = current
    , userLastSubmission = current }

data Retrieve = Retrieve
  { uid :: Int
  , username :: T.Text
  , description :: T.Text
  , isAdmin :: Bool
  , createdAt :: UTCTime
  , modifiedAt :: UTCTime 
  , lastSubmission :: UTCTime
  } deriving (Show, Generic, JSON.FromJSON, JSON.ToJSON)

toRetrieve :: DB.Entity Entry -> Retrieve
toRetrieve (DB.Entity uid User{..}) = Retrieve
  { uid = DB.fromId uid
  , username = userUsername
  , description = userDescription
  , isAdmin = userIsAdmin
  , createdAt = userCreatedAt
  , modifiedAt = userModifiedAt
  , lastSubmission = userLastSubmission }

data Update = Update
  { username :: T.Text
  , description :: T.Text
  , isAdmin :: Bool
  , password :: Maybe (T.Text, T.Text)
  } deriving (Show, Generic, JSON.FromJSON, JSON.ToJSON)

toUpdate :: Update -> Handler [DB.Update Entry]
toUpdate Update{..} = do
  Check.validUsername username
  -- Handle password change.
  passwordChange <-
    case password of
      Nothing -> pure []
      Just (old, new) -> do
        newHash <- liftIO $ BCrypt.hashPassword 10 (T.encodeUtf8 new)
        pure [UserPasswordHash =. T.decodeUtf8 newHash]
  current <- liftIO getCurrentTime
  pure $
    passwordChange ++
    [ UserUsername =. username
    , UserDescription =. description
    , UserIsAdmin =. isAdmin
    , UserModifiedAt =. current ]

data SortByField
  = Username
  | CreatedAt
  | LastSubmission
  deriving (Show, Generic, JSON.FromJSON, JSON.ToJSON)

toSelectOpt :: Api.SortBy SortByField -> DB.SelectOpt Entry
toSelectOpt (Api.Ascending Username) = DB.Asc UserUsername
toSelectOpt (Api.Ascending CreatedAt) = DB.Asc UserCreatedAt
toSelectOpt (Api.Ascending LastSubmission) = DB.Asc UserLastSubmission
toSelectOpt (Api.Descending Username) = DB.Desc UserUsername
toSelectOpt (Api.Descending CreatedAt) = DB.Desc UserCreatedAt
toSelectOpt (Api.Descending LastSubmission) = DB.Desc UserLastSubmission

data Criteria = Criteria
  { username :: [T.Text]
  , isAdmin :: Maybe Bool
  , createdAt :: Api.Range UTCTime
  , sortBy :: Api.SortBy SortByField
  } deriving (Show, Generic, JSON.FromJSON, JSON.ToJSON)

toFilter :: Int -> Criteria -> ([DB.Filter Entry], [DB.SelectOpt Entry])
toFilter page Criteria{..} =
  ( [ UserIsAdmin ==?. isAdmin
    , UserCreatedAt `withinRange` createdAt
    , (UserUsername `icontain`) <$> username
      & DB.FilterAnd
    ]
  , [ DB.LimitTo Conf.paginationLimit
    , DB.OffsetBy (Conf.paginationLimit * page)
    , toSelectOpt sortBy
    ]
  )
