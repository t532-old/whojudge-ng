{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators         #-}

module Whojudge.Api.Types where

import qualified Data.Text as T
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
