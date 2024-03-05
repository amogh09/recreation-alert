module Recreation.Client (fetchCampgroundForRange) where

import Control.Exception (Handler (Handler), catches, throw)
import Control.Monad.Catch (MonadThrow (throwM))
import qualified Data.ByteString.Char8 as BC8
import Data.Time (UTCTime (UTCTime), defaultTimeLocale, formatTime)
import Data.Time.Calendar.Month (Month, fromMonthDayValid)
import Network.HTTP.Client (HttpException)
import Network.HTTP.Client.Conduit (Request, setQueryString, setRequestCheckStatus)
import Network.HTTP.Simple (getResponseBody, httpJSON, parseRequest)
import Recreation.Client.Internal
import Recreation.Client.Internal.ApiCampground (ApiCampground)
import Recreation.Types.CampgroundSearch (CampgroundSearch)
import qualified Recreation.Types.CampgroundSearch as CampgroundSearch
import Recreation.Types.Campsite (Campsite)
import qualified Recreation.Types.StringException as StringException

-- Fetches campsites for the campground in the given date range.
-- Throws ApiException on any API failures.
fetchCampgroundForRange :: CampgroundSearch -> IO [Campsite]
fetchCampgroundForRange c =
  ( fmap mconcat
      . mapM (fetchCampground c)
      $ monthsInRange (CampgroundSearch.startDate c) (CampgroundSearch.endDate c)
  )
    `catches` [Handler h]
  where
    h :: HttpException -> IO [Campsite]
    h ex = throw $ ApiException ex

fetchCampground :: CampgroundSearch -> Month -> IO [Campsite]
fetchCampground c month = fetchApiCampground c month >>= either fail pure . apiCampgroundToCampsites

fetchApiCampground :: CampgroundSearch -> Month -> IO ApiCampground
fetchApiCampground c month = fetchCampgroundReq c month >>= fmap getResponseBody . httpJSON

fetchCampgroundReq :: MonadThrow m => CampgroundSearch -> Month -> m Request
fetchCampgroundReq c month = do
  day <-
    maybe
      (throwM . StringException.make $ "failed to convert month " <> show month <> " to day")
      pure
      $ fromMonthDayValid month 1
  let dayStr = BC8.pack . formatTime defaultTimeLocale dateFormat $ UTCTime day 0
  fmap
    (setRequestCheckStatus . setQueryString [("start_date", Just dayStr)])
    . parseRequest
    $ "https://www.recreation.gov/api/camps/availability/campground/"
      <> CampgroundSearch.id c
      <> "/month"
