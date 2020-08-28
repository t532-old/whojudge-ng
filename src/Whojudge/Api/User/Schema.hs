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
import           Crypto.KDF.BCrypt
import qualified Data.Aeson as JSON
import           Data.Char
import           Data.Function
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Time
import           Database.Persist as DB
import           GHC.Generics
import           Servant
import           Whojudge.Api.Types
import           Whojudge.Api.Util
import           Whojudge.Config
import           Whojudge.Database.Schema
import           Whojudge.Database.Util

type UserPoint =
  Authorized :>
    Point "users"
      UserCreation
      UserRetrieve
      UserUpdate
      UserCriteria

data UserCreation = UserCreation
  { username :: T.Text
  , password :: T.Text
  } deriving (Show, Generic, JSON.FromJSON, JSON.ToJSON)

toUser :: UserCreation -> Handler User
toUser UserCreation{..} = do
  passwordHash <- liftIO $ hashPassword 10 (T.encodeUtf8 password)
  current <- liftIO getCurrentTime
  pure User
    { userUsername = username
    , userPasswordHash = T.decodeUtf8 passwordHash
    , userDescription = ""
    , userIsAdmin = False
    , userCreatedAt = current
    , userModifiedAt = current
    , userLastSubmission = current }

data UserRetrieve = UserRetrieve
  { uid :: Int
  , username :: T.Text
  , description :: T.Text
  , isAdmin :: Bool
  , createdAt :: UTCTime
  , modifiedAt :: UTCTime 
  , lastSubmission :: UTCTime
  } deriving (Show, Generic, JSON.FromJSON, JSON.ToJSON)

toUserRetrieve :: Entity User -> UserRetrieve
toUserRetrieve (Entity uid User{..}) = UserRetrieve
  { uid = fromId uid
  , username = userUsername
  , description = userDescription
  , isAdmin = userIsAdmin
  , createdAt = userCreatedAt
  , modifiedAt = userModifiedAt
  , lastSubmission = userLastSubmission }

data UserUpdate = UserUpdate
  { username :: T.Text
  , description :: T.Text
  , isAdmin :: Maybe Bool
  , password :: Maybe (T.Text, T.Text)
  } deriving (Show, Generic, JSON.FromJSON, JSON.ToJSON)

toUserUpdate :: UserUpdate -> Handler [DB.Update User]
toUserUpdate UserUpdate{..} = do
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
        newHash <- liftIO $ hashPassword 10 (T.encodeUtf8 new)
        pure [UserPasswordHash DB.=. T.decodeUtf8 newHash]
  current <- liftIO getCurrentTime
  pure $
    adminChange ++
    passwordChange ++
    [ UserUsername DB.=. username
    , UserDescription DB.=. description
    , UserModifiedAt DB.=. current ]

data UserSortByField
  = Username
  | IsAdmin
  | CreatedAt
  | LastSubmission
  deriving (Show, Generic, JSON.FromJSON, JSON.ToJSON)

data UserSortBy
  = Ascending UserSortByField
  | Descending UserSortByField
  deriving (Show, Generic, JSON.FromJSON, JSON.ToJSON)

toSelectOptUser :: UserSortBy -> SelectOpt User
toSelectOptUser (Ascending Username) = DB.Asc UserUsername
toSelectOptUser (Ascending IsAdmin) = DB.Asc UserIsAdmin
toSelectOptUser (Ascending CreatedAt) = DB.Asc UserCreatedAt
toSelectOptUser (Ascending LastSubmission) = DB.Asc UserLastSubmission
toSelectOptUser (Descending Username) = DB.Desc UserUsername
toSelectOptUser (Descending IsAdmin) = DB.Desc UserIsAdmin
toSelectOptUser (Descending CreatedAt) = DB.Desc UserCreatedAt
toSelectOptUser (Descending LastSubmission) = DB.Desc UserLastSubmission

data UserCriteria = UserCriteria
  { username :: [T.Text]
  , isAdmin :: Maybe Bool
  , sortBy :: UserSortBy
  } deriving (Show, Generic, JSON.FromJSON, JSON.ToJSON)

toFilterUser :: Int -> UserCriteria -> ([Filter User], [SelectOpt User])
toFilterUser page UserCriteria{..} =
  ( case isAdmin of
      Just x -> [UserIsAdmin DB.==. x]
      Nothing -> []
    ++ ((UserUsername `ilike`) <$> username)
  , [ DB.LimitTo paginationLimit
    , DB.OffsetBy (paginationLimit * page)
    , toSelectOptUser sortBy
    ]
  )
