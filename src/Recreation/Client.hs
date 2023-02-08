module Recreation.Client () where

import Data.Bifunctor (Bifunctor (bimap))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Time
import Recreation.Types

data ApiCampsite = ApiCampsite
  { campsite_id :: !String,
    site :: !String,
    availabilities :: Map String String
  }

type ApiCampsites = Map String ApiCampsite

toCampsite :: ApiCampsite -> Either String Campsite
toCampsite ac =
  Campsite
    <$> pure ac.campsite_id
    <*> pure ac.site
    <*> pure
      ( fmap
          (bimap parseDay parseAvailability)
          (Map.toList ac.availabilities)
      )

parseDay :: String -> Day
parseDay = undefined

parseAvailability :: String -> Availability
parseAvailability = undefined
