{-# LANGUAGE OverloadedStrings #-}

module Whojudge.Config where

import           Data.Char
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BS
import qualified Database.Persist.Postgresql as DB

-- Connection string to your PostgreSQL database.
connectionString :: DB.ConnectionString
connectionString = ""

-- Number of connections in the connection pol.
numConnections :: Int
numConnections = 10

-- The port at which your Whojudge will be running.
appPort :: Int
appPort = 8080

-- Password for an auto-created admin user "root".
-- Do NOT change root user's username, ortherwise a new root will be created on the next restart.
rootPassword :: T.Text
rootPassword = "default_root_password_change_me"

-- Allow normal users to create Scopes.
allowNonAdminScopeCreation :: Bool
allowNonAdminScopeCreation = False

-- Allow normal users to make Scopes listed.
allowNonAdminScopeListed :: Bool
allowNonAdminScopeListed = False

-- Max problem number allowed in contest mode.
contestModeMaxProblems :: Int
contestModeMaxProblems = 30

-- Number of results per page, when searching for any resource.
paginationLimit :: Int
paginationLimit = 30

-- A boolean predicate used to check a username is valid or not.
usernamePredicate :: T.Text -> Bool
usernamePredicate x
  =  T.all (\c -> isAlphaNum c || c == '_') x
  && T.length x > 2
  && T.length x < 33

-- The corresponding error message for an illegal username.
usernameTip :: BS.ByteString
usernameTip = "Username should only contain letters, numbers and underscores, with length greater than 2 and less than 33."

-- A boolean predicate used to check a password is valid or not.
passwordPredicate :: T.Text -> Bool
passwordPredicate x
  =  T.all (\c -> ord c > 31 && ord c < 127) x
  && T.length x > 5
  && T.length x < 65

-- The corresponding error message for an illegal password.
passwordTip :: BS.ByteString
passwordTip = "Password should only contain visible ASCII characters, with length greater than 5 and less than 65."
