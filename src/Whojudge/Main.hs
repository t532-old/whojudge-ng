{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE Rank2Types     #-}
{-# LANGUAGE TypeOperators  #-}

module Whojudge.Main where

import           Control.Monad.IO.Class
import           Control.Monad.Logger
import qualified Data.Aeson as JSON
import           Data.Pool
import qualified Database.Persist.Postgresql as DB
import qualified Network.Wai.Handler.Warp as Warp
import           Servant
import qualified Whojudge.Api.User.Endpoint as User
import qualified Whojudge.Api.User.Schema as User
import qualified Whojudge.Config as Conf
import           Whojudge.Database.Schema
import qualified Whojudge.Database.Util as DB

type Api = User.Point

server :: DB.Handle -> Server Api
server perform = User.server perform

apiProxy :: Proxy Api
apiProxy = Proxy

app :: DB.Handle -> Application
app db = serve apiProxy (server db)

main :: IO ()
main = runStdoutLoggingT . DB.withPostgresqlPool Conf.connectionString Conf.numConnections $ \pool -> do
  withResource pool (DB.runSqlConn $ DB.runMigration migrateAll)
  liftIO $ Warp.run Conf.appPort (app (liftIO . (flip DB.runSqlPool pool)))
