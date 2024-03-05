module Recreation.Client.Internal where

import Control.Exception (Exception)
import Data.Aeson (FromJSON)
import Data.Bifunctor (bimap)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Time (Day, DayPeriod (dayPeriod), UTCTime (utctDay))
import Data.Time.Calendar.Month (Month)
import Data.Time.Format.ISO8601 (iso8601ParseM)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Recreation.Types

data ApiCampsite = ApiCampsite
  { campsite_id :: !String,
    site :: !String,
    availabilities :: Map String String
  }
  deriving (Show, Generic)

newtype ApiCampground = ApiCampsites
  { campsites :: Map String ApiCampsite
  }
  deriving (Show, Generic)

instance FromJSON ApiCampsite

instance FromJSON ApiCampground

newtype ApiException e = ApiException e

instance Show e => Show (ApiException e) where
  show (ApiException e) = "API request failed: " <> show e

instance (Typeable e, Show e) => Exception (ApiException e)

dateFormat :: String
dateFormat = "%FT%T.000Z"

apiCampgroundToCampsites :: ApiCampground -> Either String [Campsite]
apiCampgroundToCampsites = mapM toCampsite . fmap snd . Map.toList . campsites

monthsInRange :: StartDate -> EndDate -> [Month]
monthsInRange s e = uniqAsc . fmap dayPeriod $ [s .. e]

uniqAsc :: (Eq a, Ord a) => [a] -> [a]
uniqAsc = Set.toAscList . Set.fromList

toCampsite :: ApiCampsite -> Either String Campsite
toCampsite ac =
  fmap
    (Campsite (ac.campsite_id) (ac.site))
    ( mapM tupleEither
        . fmap (bimap parseDay parseAvailability)
        $ Map.toList (ac.availabilities)
    )

parseDay :: String -> Either String Day
parseDay x =
  fmap utctDay
    . maybe (Left $ "failed to parse " <> x) Right
    . iso8601ParseM
    $ x

parseAvailability :: String -> Either String Availability
parseAvailability "Available" = Right Available
parseAvailability _ = Right NotAvailable

tupleEither :: (Either a b, Either a c) -> Either a (b, c)
tupleEither (Left e, _) = Left e
tupleEither (_, Left e) = Left e
tupleEither (Right x, Right y) = Right (x, y)
