{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Whojudge.Database.Schema where

import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Aeson
import Data.Pool
import Data.Text
import Data.Time
import Database.Persist
import Database.Persist.Postgresql
import Database.Persist.TH
import GHC.Generics

share [mkPersist sqlSettings, mkDeleteCascade sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|

  -- A user to a WhoJudge.
  -- User is the only entity NOT specific to Scopes. i.e. Users are global entities.
  -- A user is either a normal user or an admin.
  User

    username Text
    passwordHash Text  -- Password is hashed through the bcrypt algorithm.
    description Text   -- This description can be parsed in any way preferred (it is parsed as markdown in twilight)
    isAdmin Bool       -- Admin has implicit ownership to every scope, and can modify and delete any user
    createdAt UTCTime
    modifiedAt UTCTime
    lastSubmission UTCTime

    UniqueUsername username    -- Username should be unique, to avoid imposters

    deriving Show Generic FromJSON ToJSON

  ---------------------------------

  -- A submission made by a user.
  -- This includes the original updated source and the judge result.
  -- A Submission to a Problem in a certain Scope can be made only if the User has a Participance to the Scope.
  Submission

    user UserId         -- User -[oneToMany]-> Submission
    problem ProblemId   -- Problem -[oneToMany]-> Submission
    source Text       -- The original source code
    createdAt UTCTime
    modifiedAt UTCTime

    deriving Show Generic FromJSON ToJSON

  ---------------------------------

  -- a set of problems plus extra organizing facilities.
  -- Scope provides abstraction for normal problem lists, large problem libraries and also contests.
  -- Scope is the basic organizing unit in a WhoJudge system, all other entities (except User) are specific to Scopes.
  Scope

    description Text                     -- This description can be parsed in any way preferred (it is parsed as markdown in twilight)
    startTime UTCTime Maybe                -- If set, the problems can only be submitted after the start time
    endTime UTCTime Maybe                  -- If set, the problems can not be submitted after the end time and new participants are no longer allowed
    hasLeaderboard Bool                    -- If True, the scope will have a leaderboard. Unavailable for scopes with >50 problems
    -- Visibility options
    listed Bool                            -- Listed and can be found by search
    invitationOnly Bool                    -- Cannot join unless invited
    participantListingViewOnly Bool        -- Allow only participated user to view the listing
    participantAnnouncementiewOnly Bool    -- Allow only participated user to view the announcements
    participantLeaderboardViewOnly Bool    -- Allow only participated user to view the leaderboard (if has one)
    viewListingAfterStartedOnly Bool       -- Allow viewing listing only after startTime
    viewAnnouncementAfterStartedOnly Bool  -- Allow viewing announcements only after startTime
    createdAt UTCTime
    modifiedAt UTCTime

    deriving Show Generic FromJSON ToJSON

  --------------------------------------

  -- A Problem 
  Problem

    scope ScopeId  -- Problems should be modifiable by all users with an Ownership to the Scope
    content Text           -- Content is mere Text and can be interpreted in any format for max flexibility (parsed as markdown in twilight)
    createdAt UTCTime
    modifiedAt UTCTime
    -- Visibility settings
    listed Bool              -- Listed and can be searched
    visible Bool             -- Can be read by non-owners
    -- Requirement settings
    requireList [ProblemId]  -- Require some of these problems to be solved
    requireNum Int           -- Require how many items of the list to be solved

    deriving Show Generic FromJSON ToJSON

  --------------------------------------

  -- Represents a User's participance to a Scope.
  -- A User with a Participance to a specific Scope is able to:
  -- * View problem listing;
  -- * View problems;
  -- * Create submissions;
  -- * View leadearboards (if in contest mode);
  Participance

    user UserId              -- User -[oneToMany]-> Participance
    scope ScopeId  -- Scope -[oneToMany]-> Participance
    createdAt UTCTime
    modifiedAt UTCTime

    UniqueParticipance user scope  -- A Participance is uniquely identified by its participant user and the participated scope

    deriving Show Generic FromJSON ToJSON

  ---------------------------------------

  -- Ownership to a scope. The creater of a scope naturally has an Ownership, and every admin has an implicit ownership.
  -- A user with an Ownership to a Scope have complete control to the Scope. They can:
  -- * Automatically obtain an implicit Participance;
  -- * Delete the Scope;
  -- * Add/delete participants;
  -- * Add/delete/edit problems;
  -- * edit contest and visibility settings.
  -- * Grant/revoke ownership to other users. (Note this!)
  Ownership

    user UserId              -- User -[oneToMany]-> Ownership
    scope ScopeId  -- Scope -[oneToMany]-> Ownership
    createdAt UTCTime
    modifiedAt UTCTime

    UniqueOwnership user scope  -- An Ownership is uniquely identified by the owner user and the owned scope

    deriving Show Generic FromJSON ToJSON

  ----------------------------------------

  -- An announcement is a message linked to a Scope.
  Announcement
  
    scope ScopeId  -- Scope -[oneToMany]-> Ownership
    content Text
    createdAt Text
    modifiedAt UTCTime
    visible Bool             -- Can be read by non-owners

    deriving Show Generic FromJSON ToJSON
  
  ----------------------------------------
  
  -- An authorization of a user is a token obtained by a user
  -- and can be revoked by the user themselves.
  Authorization

    token Text
    user UserId

    AuthToken token

    deriving Show Generic FromJSON ToJSON

|]
