{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE Rank2Types     #-}
{-# LANGUAGE TypeOperators  #-}

module Whojudge.Main where

import           Control.Monad.Logger
import           Control.Monad.Reader
import qualified Data.Aeson as JSON
import           Data.Pool
import qualified Database.Persist.Postgresql as DB
import           Network.Wai.Handler.Warp
import           Servant
import           Whojudge.Api.User.Endpoint
import           Whojudge.Api.User.Schema
import           Whojudge.Config
import           Whojudge.Database.Schema
import           Whojudge.Database.Util

type Api = UserPoint

server :: Database -> Server Api
server perform = userServer perform

apiProxy :: Proxy Api
apiProxy = Proxy

app :: Database -> Application
app db = serve apiProxy (server db)

main :: IO ()
main = runStdoutLoggingT . DB.withPostgresqlPool connectionString numConnections $ \pool -> do
  withResource pool (DB.runSqlConn $ DB.runMigration migrateAll)
  liftIO $ run appPort (app (liftIO . (flip DB.runSqlPool pool)))
