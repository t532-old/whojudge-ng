{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE Rank2Types          #-}

module Whojudge.Database.Util where

import           Control.Monad.IO.Class
import           Control.Monad.Reader
import qualified Data.Aeson as JSON
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Database.Persist as DB
import qualified Database.Persist.Postgresql.JSON as DB
import qualified Database.Persist.Sql as DB
import qualified Whojudge.Api.Types as Api

type Handle = forall m a. MonadIO m => ReaderT DB.SqlBackend IO a -> m a

toId :: (DB.ToBackendKey DB.SqlBackend record) => Int -> DB.Key record
toId a = DB.toSqlKey (fromIntegral a)

fromId :: (DB.ToBackendKey DB.SqlBackend record) => DB.Key record -> Int
fromId a = fromIntegral (DB.fromSqlKey a)

alwaysTrue :: DB.Filter record
alwaysTrue = DB.FilterAnd []

type UpdateCombinator = forall record typ. DB.PersistField typ => DB.EntityField record typ -> typ -> DB.Update record

type FilterCombinator = forall record typ. DB.PersistField typ => DB.EntityField record typ -> typ -> DB.Filter record

type FilterCombinatorOf a = forall record. DB.EntityField record a -> a -> DB.Filter record

type FilterCombinatorBy t = forall record typ. DB.PersistField typ => DB.EntityField record typ -> t typ -> DB.Filter record

maybeify :: FilterCombinator -> FilterCombinatorBy Maybe
maybeify _ field Nothing = alwaysTrue
maybeify (~.) field (Just x) = field ~. x

(=.) :: UpdateCombinator
(=.) = (DB.=.)

(==.) :: FilterCombinator
(==.) = (DB.==.)

(>=.) :: FilterCombinator
(>=.) = (DB.>=.)

(<=.) :: FilterCombinator
(<=.) = (DB.<=.)

(@>.) :: FilterCombinatorOf JSON.Value
(@>.) = (DB.@>.)

(==?.) :: FilterCombinatorBy Maybe
(==?.) = maybeify (==.)

(>=?.) :: FilterCombinatorBy Maybe
(>=?.) = maybeify (>=.)

(<=?.) :: FilterCombinatorBy Maybe
(<=?.) = maybeify (<=.)

withinRange :: FilterCombinatorBy Api.Range
_ `withinRange` (Nothing, Nothing) = alwaysTrue
field `withinRange` (Just x, Nothing) = field >=. x
field `withinRange` (Nothing, Just y) = field <=. y
field `withinRange` (Just x, Just y) = DB.FilterAnd
  [ field >=. x
  , field <=. y
  ]

icontain :: FilterCombinatorOf T.Text
field `icontain` val = DB.Filter field (DB.FilterValue $ T.concat ["%", val, "%"]) (DB.BackendSpecificFilter " ILIKE ")

textVectorToValue :: V.Vector T.Text -> JSON.Value
textVectorToValue = JSON.Array . (JSON.String <$>)

-- NOT Total, but suffices in this case
valueToTextVector :: JSON.Value -> V.Vector T.Text
valueToTextVector (JSON.Array xs) = unString <$> xs
  where
    unString (JSON.String x) = x
    unString _ = error "unString nonstring"
valueToTextVector _ = error "valueToTextVector nonarray"
