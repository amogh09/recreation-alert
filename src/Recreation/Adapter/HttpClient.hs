module Recreation.Adapter.HttpClient
  ( toCampsite,
    ApiCampsite,
    fetchCampgroundForRange,
  )
where

import Control.Monad.Catch (MonadThrow (throwM))
import Data.Aeson (FromJSON)
import Data.Bifunctor (Bifunctor (bimap, first))
import qualified Data.ByteString.Char8 as BC8
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Time
  ( Day,
    DayPeriod (dayPeriod),
    UTCTime (UTCTime, utctDay),
    defaultTimeLocale,
    formatTime,
  )
import Data.Time.Calendar.Month (Month, fromMonthDayValid)
import Data.Time.Format.ISO8601 (iso8601ParseM)
import GHC.Generics (Generic)
import Network.HTTP.Client.Conduit (Request, setQueryString, setRequestCheckStatus)
import Network.HTTP.Simple (getResponseBody, httpJSON, parseRequest)
import Recreation.Core.Types
  ( Availability (..),
    Campground (..),
    Campsite (Campsite),
    EndDate,
    StartDate,
  )
import UnliftIO (fromEither, stringException)

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

dateFormat :: String
dateFormat = "%FT%T.000Z"

monthsInRange :: StartDate -> EndDate -> [Month]
monthsInRange s e = uniqAsc . fmap dayPeriod $ [s .. e]

uniqAsc :: (Eq a, Ord a) => [a] -> [a]
uniqAsc = Set.toAscList . Set.fromList

fetchCampgroundForRange :: Campground -> Day -> Day -> IO [Campsite]
fetchCampgroundForRange c s e =
  fmap mconcat . mapM (fetchCampground c) $ monthsInRange s e

fetchCampground :: Campground -> Month -> IO [Campsite]
fetchCampground c month =
  fetchApiCampground c month
    >>= fromEither . first stringException . apiCampgroundToCampsites

fetchApiCampground :: Campground -> Month -> IO ApiCampground
fetchApiCampground c month =
  fetchCampgroundReq c month >>= fmap getResponseBody . httpJSON

fetchCampgroundReq ::
  MonadThrow m => Campground -> Month -> m Request
fetchCampgroundReq c month = do
  day <-
    maybe
      (throwM . stringException $ "failed to convert month " <> show month <> " to day")
      pure
      $ fromMonthDayValid month 1
  let dayStr = BC8.pack . formatTime defaultTimeLocale dateFormat $ UTCTime day 0
  fmap
    (setRequestCheckStatus . setQueryString [("start_date", Just dayStr)])
    . parseRequest
    $ "https://www.recreation.gov/api/camps/availability/campground/" <> c.id <> "/month"

apiCampgroundToCampsites :: ApiCampground -> Either String [Campsite]
apiCampgroundToCampsites = mapM toCampsite . fmap snd . Map.toList . campsites

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
