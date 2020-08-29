{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE Rank2Types        #-}
{-# LANGUAGE OverloadedStrings #-}

module Whojudge.Database.Util where

import           Control.Monad.IO.Class
import           Control.Monad.Reader
import qualified Data.Text as T
import qualified Database.Persist as DB
import qualified Database.Persist.Sql as DB

type Handle = forall m a. MonadIO m => ReaderT DB.SqlBackend IO a -> m a

toId :: (DB.ToBackendKey DB.SqlBackend record) => Int -> DB.Key record
toId a = DB.toSqlKey (fromIntegral a)

fromId :: (DB.ToBackendKey DB.SqlBackend record) => DB.Key record -> Int
fromId a = fromIntegral (DB.fromSqlKey a)

ilike :: DB.EntityField record T.Text -> T.Text -> DB.Filter record
field `ilike` val = DB.Filter field (DB.FilterValue $ T.concat ["%", val, "%"]) (DB.BackendSpecificFilter "ILIKE")
