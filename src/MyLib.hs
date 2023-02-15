module MyLib (go) where

import Control.Monad (forM_)
import Data.Functor.Contravariant (Predicate (Predicate))
import Data.Time (Day)
import Recreation.Class (Notifier (..), RecreationClient (..))
import Recreation.Types
  ( Campground (..),
    Campsite (availabilities),
    EndDate,
    StartDate,
    isAvailable,
    mapAvailabilities,
  )
import UnliftIO (MonadIO)

go ::
  ( RecreationClient m,
    Notifier m,
    MonadIO m
  ) =>
  [Campground] ->
  StartDate ->
  EndDate ->
  m ()
go cs s e = forM_ cs (\c -> goOnce c s e)

goOnce ::
  (Monad m, RecreationClient m, Notifier m) =>
  Campground ->
  StartDate ->
  EndDate ->
  m ()
goOnce ground s e = do
  campsites <-
    availableCampsites ground.campsitePredicate ground.dayPredicate
      <$> getCampgroundAvailability ground s e
  if null campsites
    then notifyNoAvailability ground
    else notifyAvailability ground campsites

availableCampsites ::
  Predicate Campsite -> Predicate Day -> [Campsite] -> [Campsite]
availableCampsites (Predicate cp) (Predicate dp) =
  filter (not . null . availabilities)
    . map (mapAvailabilities $ filter (isAvailable . snd) . filter (dp . fst))
    . filter cp
