{-# LANGUAGE OverloadedStrings #-}

module Whojudge.Api.Error where

import Servant

notExist, notVerified, notOwner, notAdmin, wrongPassword, notLoggedIn :: ServerError

notExist = err404 {errBody = "The resource identified by this ID does not exist."}

notVerified = err401 {errBody = "Your account is not yet verified."}

notOwner = err401 {errBody = "This resource does not belong to you, so you cannot perform this action."}

notAdmin = err401 {errBody = "You are not admin, so you cannot perform this action."}

wrongPassword = err400 {errBody = "Wrong password."}

notLoggedIn = err401 {errBody = "You are not logged in."}
