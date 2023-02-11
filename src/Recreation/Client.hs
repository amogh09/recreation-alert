module Recreation.Client (toCampsite, ApiCampsite) where

import Control.Monad.Catch (MonadThrow)
import Data.Aeson (FromJSON)
import Data.Bifunctor (Bifunctor (bimap, first))
import qualified Data.ByteString.Char8 as BC8
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Time
import Data.Time.Format.ISO8601 (ISO8601 (iso8601Format), formatParseM, iso8601ParseM, iso8601Show)
import GHC.Generics (Generic)
import Network.HTTP.Client.Conduit (Request, setQueryString, setRequestCheckStatus)
import Network.HTTP.Simple (getResponseBody, httpJSON, parseRequest)
import Network.URI.Encode (encodeByteString)
import qualified Network.URI.Encode as URI
import Recreation.Types
import UnliftIO

data ApiCampsite = ApiCampsite
  { campsite_id :: !String,
    site :: !String,
    availabilities :: Map String String
  }
  deriving (Show, Generic)

data ApiCampground = ApiCampsites
  { campsites :: Map String ApiCampsite
  }
  deriving (Show, Generic)

instance FromJSON ApiCampsite

instance FromJSON ApiCampground

dateFormat :: String
dateFormat = "%FT%T.000Z"

fetchCampground :: CampgroundId -> Day -> IO [Campsite]
fetchCampground cid day =
  fetchApiCampground cid day
    >>= fromEither . first stringException . apiCampgroundToCampsites

fetchApiCampground :: CampgroundId -> Day -> IO ApiCampground
fetchApiCampground cid day =
  fetchCampgroundReq cid day >>= fmap getResponseBody . httpJSON

fetchCampgroundReq ::
  MonadThrow m => CampgroundId -> Day -> m Request
fetchCampgroundReq cid day = do
  let dayStr = BC8.pack . formatTime defaultTimeLocale dateFormat $ UTCTime day 0
  fmap setRequestCheckStatus
    . fmap (setQueryString [("start_date", Just dayStr)])
    . parseRequest
    $ "https://www.recreation.gov/api/camps/availability/campground/" <> cid <> "/month"

apiCampgroundToCampsites :: ApiCampground -> Either String [Campsite]
apiCampgroundToCampsites = mapM toCampsite . fmap snd . Map.toList . campsites

toCampsite :: ApiCampsite -> Either String Campsite
toCampsite ac =
  Campsite
    <$> pure ac.campsite_id
    <*> pure ac.site
    <*> ( mapM tupleEither
            . fmap (bimap parseDay parseAvailability)
            $ Map.toList ac.availabilities
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
