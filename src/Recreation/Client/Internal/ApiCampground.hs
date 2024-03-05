module Recreation.Client.Internal.ApiCampground where

import Data.Aeson (FromJSON)
import Data.Map (Map)
import GHC.Generics (Generic)
import Recreation.Client.Internal.ApiCampsite (ApiCampsite)

newtype ApiCampground = ApiCampsites
  { campsites :: Map String ApiCampsite
  }
  deriving (Show, Generic)

instance FromJSON ApiCampground
