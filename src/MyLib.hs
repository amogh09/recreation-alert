module MyLib (go) where

import Control.Monad (forever)
import Data.Functor.Contravariant (Predicate (Predicate))
import Data.Time (Day)
import Recreation.Class (Notifier (..), RecreationClient (..))
import Recreation.Types
  ( Campground,
    Campsite (availabilities),
    EndDate,
    StartDate,
    isAvailable,
    mapAvailabilities,
  )
import UnliftIO (MonadIO)
import UnliftIO.Concurrent (threadDelay)

go ::
  ( RecreationClient m,
    Notifier m,
    MonadIO m
  ) =>
  Predicate Campsite ->
  Predicate Day ->
  Campground ->
  StartDate ->
  EndDate ->
  m ()
go cp dp c s e =
  forever $ goOnce cp dp c s e >> threadDelay (minutes 2)

minutes :: Int -> Int
minutes = (* 60) . (* 1000000)

goOnce ::
  (Monad m, RecreationClient m, Notifier m) =>
  Predicate Campsite ->
  Predicate Day ->
  Campground ->
  StartDate ->
  EndDate ->
  m ()
goOnce cp dp campground s e = do
  campsites <-
    availableCampsites cp dp
      <$> getCampgroundAvailability campground s e
  if null campsites
    then notifyNoAvailability
    else notifyAvailability campground campsites

availableCampsites ::
  Predicate Campsite -> Predicate Day -> [Campsite] -> [Campsite]
availableCampsites (Predicate cp) (Predicate dp) =
  filter (not . null . availabilities)
    . map (mapAvailabilities $ filter (isAvailable . snd) . filter (dp . fst))
    . filter cp
