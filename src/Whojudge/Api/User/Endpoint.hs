{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeApplications      #-}

module Whojudge.Api.User.Endpoint where

import qualified Crypto.KDF.BCrypt as BCrypt
import qualified Data.Aeson as JSON
import           Data.Function
import           Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Time
import qualified Database.Persist as DB
import           Servant
import qualified Whojudge.Api.Checker as Check
import qualified Whojudge.Api.Error as Err
import           Whojudge.Api.User.Schema
import           Whojudge.Database.Schema
import qualified Whojudge.Database.Util as DB

server :: DB.Handle -> Server Point
server perform auth =
  create :<|>
  retrieve :<|>
  update :<|>
  delete :<|>
  list where

  create :: Creation -> Handler NoContent
  create cre = do
    -- TODO: Add CAPTCHA and mail verification
    -- Create and insert
    newUser <- toEntry cre
    (perform $ DB.insertBy newUser)
      >>= (& Check.left err403 {errBody = "This username is already used."})
    pure NoContent

  retrieve :: Int -> Handler Retrieve
  retrieve uid = do
    -- Auth
    Check.auth perform auth
    -- Retrieve
    user <- (perform $ DB.get $ DB.toId @User uid)
      >>= (& Check.nothing Err.notExist)
    -- Truncate and return
    pure $ toRetrieve $ DB.Entity (DB.toId uid) user
  
  update :: Int -> Update -> Handler NoContent
  update uid upd@Update{..} = do
    -- Auth
    (operId, oper) <- Check.auth perform auth
    operId == DB.toId uid || userIsAdmin oper
      & Check.false Err.notOwner
    not isAdmin || userIsAdmin oper
      & Check.false Err.notAdmin
    isNothing password || BCrypt.validatePassword
      (T.encodeUtf8 $ fst $ fromJust password)
      (T.encodeUtf8 $ userPasswordHash oper)
      & Check.false Err.wrongPassword
    updList <- toUpdate upd
    perform $ DB.update (DB.toId @User uid) updList
    -- End
    pure NoContent

  delete :: Int -> Handler NoContent
  delete uid = do
    -- Auth
    (operId, oper) <- Check.auth perform auth
    -- Permission
    operId == DB.toId uid || userIsAdmin oper
      & Check.false Err.notOwner
    -- Delete.
    -- It is possible to delete a user that does not exist, and nothing will happen in this case.
    -- Note that ALL resources linked to the user will be deleted, incl. ownerships, participances
    -- and submissions. (Cascade deletion)
    perform $ DB.deleteCascade (DB.toId @User uid)
    pure NoContent

  list :: Int -> Criteria -> Handler [Retrieve]
  list page cri = do
    Check.auth perform auth
    userEnts <- perform $ uncurry DB.selectList $ toFilter page cri
    pure $ toRetrieve <$> userEnts
