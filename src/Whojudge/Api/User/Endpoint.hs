{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE BlockArguments        #-}

module Whojudge.Api.User.Endpoint where

import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Crypto.KDF.BCrypt
import qualified Data.Aeson as JSON
import           Data.Function
import           Data.Functor
import           Data.Maybe
import           Data.Pool
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Time
import qualified Database.Persist as DB
import           GHC.Generics
import           Servant
import           Whojudge.Api.Types
import           Whojudge.Api.User.Schema
import           Whojudge.Api.Util
import           Whojudge.Config
import           Whojudge.Database.Schema
import           Whojudge.Database.Util

userServer :: Database -> Server UserPoint
userServer perform auth =
  create :<|>
  retrieve :<|>
  update :<|>
  delete :<|>
  list where

  create :: UserCreation -> Handler NoContent
  create cre@UserCreation{..} = do
    -- TODO: Add CAPTCHA and mail verification
    -- Validate
    usernamePredicate username
      & ifNot err400 {errBody = usernameTip}
    passwordPredicate password
      & ifNot err400 {errBody = passwordTip}
    -- Create and insert
    newUser <- toUser cre
    (perform $ DB.insertBy newUser)
      >>= (& ifLeft err403 {errBody = "This username is already used."})
    pure NoContent

  retrieve :: Int -> Handler UserRetrieve
  retrieve uid = do
    -- Auth
    checkAuth perform auth
    -- Retrieve
    user <- (perform $ DB.get $ toId @User uid)
      >>= (& ifNothing err404 {errBody = "This user does not exist."})
    -- Truncate and return
    pure $ toUserRetrieve $ DB.Entity (toId uid) user
  
  update :: Int -> UserUpdate -> Handler NoContent
  update uid upd@UserUpdate{..} = do
    -- Auth
    (operId, oper) <- checkAuth perform auth
    let operIsSelf = operId == toId uid
    operIsSelf || userIsAdmin oper
      & ifNot err401 {errBody = "Non-admins cannot modify other user's info."}
    isNothing isAdmin || userIsAdmin oper
      & ifNot err401 {errBody = "Non-admins cannot set themselves to be admin."}
    isNothing password || operIsSelf
      & ifNot err401 {errBody = "Only the user themselves can change their password."}
    isNothing password || validatePassword (T.encodeUtf8 $ fst $ fromJust password) (T.encodeUtf8 $ userPasswordHash oper)
      & ifNot err400 {errBody = "The old password is wrong."}
    updList <- toUserUpdate upd
    perform $ DB.update (toId @User uid) updList
    -- End
    pure NoContent

  delete :: Int -> Handler NoContent
  delete uid = do
    -- Auth
    (operId, oper) <- checkAuth perform auth
    -- Permission
    operId == toId uid || userIsAdmin oper
      & ifNot err401 {errBody = "You cannot delete other user."}
    -- Delete.
    -- It is possible to delete a user that does not exist, and nothing will happen in this case.
    -- Note that ALL resources linked to the user will be deleted, incl. ownerships, participances
    -- and submissions. (Cascade deletion)
    perform $ DB.deleteCascade (toId @User uid)
    pure NoContent

  list :: Int -> UserCriteria -> Handler [UserRetrieve]
  list page cri = do
    checkAuth perform auth
    userEnts <- perform $ uncurry DB.selectList $ toFilterUser page cri
    pure $ toUserRetrieve <$> userEnts
