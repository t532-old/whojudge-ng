{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeOperators         #-}

module Whojudge.Api.Types where

import qualified Data.Aeson as JSON
import qualified Data.Text as T
import           Data.Time
import           GHC.Generics
import           GHC.TypeLits
import           Servant

type Authorized = Header "Authorization" T.Text

-- A Point for CRUD.
type Point (name :: Symbol) (create :: *) (retrieve :: *) (update :: *) (criteria :: *) =
  name :> (
    ReqBody '[JSON] create :>
      Post '[JSON] NoContent :<|> -- Create
    Capture "id" Int :>
      Get '[JSON] retrieve  :<|> -- Retrieve
    Capture "id" Int :>
      ReqBody '[JSON] update :>
        Put '[JSON] NoContent :<|> -- Update
    Capture "id" Int :>
      Delete '[JSON] NoContent :<|> -- Delete
    QueryParam' '[Required, Strict] "page" Int :> -- Zero-indexed page number
      ReqBody '[JSON] criteria :>
        Post '[JSON] [retrieve] -- List.
    -- Note that here we did NOT make it full RESTful, but used POST for listing instead.
    -- This is because it is hard to encode the searching criteria as QueryParam's in a simple manner.
  )

data SortBy a
  = Ascending a
  | Descending a
deriving instance Show a => Show (SortBy a)
deriving instance Generic a => Generic (SortBy a)
instance (Generic a, JSON.FromJSON a) => JSON.FromJSON (SortBy a)
instance (Generic a, JSON.ToJSON a) => JSON.ToJSON (SortBy a)

-- Inclusive range
type Range a = (Maybe a, Maybe a)

deriving instance Generic UTCTime
