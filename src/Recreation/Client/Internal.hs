module Recreation.Client.Internal where

import Control.Exception (Exception)
import Data.Bifunctor (bimap)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Time (Day, DayPeriod (dayPeriod), UTCTime (utctDay))
import Data.Time.Calendar.Month (Month)
import Data.Time.Format.ISO8601 (iso8601ParseM)
import Data.Typeable (Typeable)
import Recreation.Client.Internal.ApiCampground (ApiCampground)
import qualified Recreation.Client.Internal.ApiCampground as ApiCampground
import Recreation.Client.Internal.ApiCampsite (ApiCampsite)
import qualified Recreation.Client.Internal.ApiCampsite as ApiCampsite
import qualified Recreation.Types.CampgroundSearch as CS
import Recreation.Types.Campsite (Campsite)
import qualified Recreation.Types.Campsite as Campsite

newtype ApiException e = ApiException e

instance Show e => Show (ApiException e) where
  show (ApiException e) = "API request failed: " <> show e

instance (Typeable e, Show e) => Exception (ApiException e)

dateFormat :: String
dateFormat = "%FT%T.000Z"

apiCampgroundToCampsites :: ApiCampground -> Either String [Campsite]
apiCampgroundToCampsites = mapM toCampsite . fmap snd . Map.toList . ApiCampground.campsites

monthsInRange :: CS.StartDate -> CS.EndDate -> [Month]
monthsInRange s e = uniqAsc . fmap dayPeriod $ [s .. e]

uniqAsc :: (Eq a, Ord a) => [a] -> [a]
uniqAsc = Set.toAscList . Set.fromList

toCampsite :: ApiCampsite -> Either String Campsite
toCampsite ac =
  fmap
    (Campsite.Campsite (ApiCampsite.campsite_id ac) (ApiCampsite.site ac))
    ( mapM tupleEither
        . fmap (bimap parseDay parseAvailability)
        $ Map.toList (ApiCampsite.availabilities ac)
    )

parseDay :: String -> Either String Day
parseDay x =
  fmap utctDay
    . maybe (Left $ "failed to parse " <> x) Right
    . iso8601ParseM
    $ x

parseAvailability :: String -> Either String Campsite.Availability
parseAvailability "Available" = Right Campsite.Available
parseAvailability _ = Right Campsite.NotAvailable

tupleEither :: (Either a b, Either a c) -> Either a (b, c)
tupleEither (Left e, _) = Left e
tupleEither (_, Left e) = Left e
tupleEither (Right x, Right y) = Right (x, y)
