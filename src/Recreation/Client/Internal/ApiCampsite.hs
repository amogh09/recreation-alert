module Recreation.Client.Internal.ApiCampsite where

import Data.Aeson (FromJSON)
import Data.Map (Map)
import GHC.Generics (Generic)

data ApiCampsite = ApiCampsite
  { campsite_id :: !String,
    site :: !String,
    availabilities :: Map String String
  }
  deriving (Show, Generic)

instance FromJSON ApiCampsite
