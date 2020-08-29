{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}

module Whojudge.Api.User.Schema where

import           Control.Monad.IO.Class
import qualified Crypto.KDF.BCrypt as BCrypt
import qualified Data.Aeson as JSON
import           Data.Function
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Time
import qualified Database.Persist as DB
import           GHC.Generics
import           Servant
import qualified Whojudge.Api.Types as Api
import qualified Whojudge.Config as Conf
import           Whojudge.Database.Schema
import qualified Whojudge.Database.Util as DB

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
  , isAdmin :: Maybe Bool
  , password :: Maybe (T.Text, T.Text)
  } deriving (Show, Generic, JSON.FromJSON, JSON.ToJSON)

toUpdate :: Update -> Handler [DB.Update Entry]
toUpdate Update{..} = do
  -- Handle admin change.
  adminChange <-
    case isAdmin of
      Nothing -> pure []
      Just flag -> pure [UserIsAdmin DB.=. flag]
  -- Handle password change.
  passwordChange <-
    case password of
      Nothing -> pure []
      Just (old, new) -> do
        newHash <- liftIO $ BCrypt.hashPassword 10 (T.encodeUtf8 new)
        pure [UserPasswordHash DB.=. T.decodeUtf8 newHash]
  current <- liftIO getCurrentTime
  pure $
    adminChange ++
    passwordChange ++
    [ UserUsername DB.=. username
    , UserDescription DB.=. description
    , UserModifiedAt DB.=. current ]

data SortByField
  = Username
  | IsAdmin
  | CreatedAt
  | LastSubmission
  deriving (Show, Generic, JSON.FromJSON, JSON.ToJSON)

data SortBy
  = Ascending SortByField
  | Descending SortByField
  deriving (Show, Generic, JSON.FromJSON, JSON.ToJSON)

toSelectOpt :: SortBy -> DB.SelectOpt Entry
toSelectOpt (Ascending Username) = DB.Asc UserUsername
toSelectOpt (Ascending IsAdmin) = DB.Asc UserIsAdmin
toSelectOpt (Ascending CreatedAt) = DB.Asc UserCreatedAt
toSelectOpt (Ascending LastSubmission) = DB.Asc UserLastSubmission
toSelectOpt (Descending Username) = DB.Desc UserUsername
toSelectOpt (Descending IsAdmin) = DB.Desc UserIsAdmin
toSelectOpt (Descending CreatedAt) = DB.Desc UserCreatedAt
toSelectOpt (Descending LastSubmission) = DB.Desc UserLastSubmission

data Criteria = Criteria
  { username :: [T.Text]
  , isAdmin :: Maybe Bool
  , sortBy :: SortBy
  } deriving (Show, Generic, JSON.FromJSON, JSON.ToJSON)

toFilter :: Int -> Criteria -> ([DB.Filter Entry], [DB.SelectOpt Entry])
toFilter page Criteria{..} =
  ( case isAdmin of
      Just x -> [UserIsAdmin DB.==. x]
      Nothing -> []
    ++ ((UserUsername `DB.ilike`) <$> username)
  , [ DB.LimitTo Conf.paginationLimit
    , DB.OffsetBy (Conf.paginationLimit * page)
    , toSelectOpt sortBy
    ]
  )
