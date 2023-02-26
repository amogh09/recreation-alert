module Recreation.Usecase.Availability (go) where

import Control.Monad (forM_)
import Recreation.Core.Predicate (availableCampsites)
import Recreation.Core.Types
  ( Campground (..),
    EndDate,
    StartDate,
  )
import Recreation.Usecase.Class (Notifier (..), RecreationClient (..), Trace, info)
import UnliftIO (MonadIO)

go ::
  ( RecreationClient m,
    Notifier m,
    MonadIO m,
    Trace m
  ) =>
  [Campground] ->
  StartDate ->
  EndDate ->
  m ()
go cs s e = forM_ cs $ \c -> goOnce c s e

goOnce ::
  (Monad m, RecreationClient m, Notifier m, Trace m) =>
  Campground ->
  StartDate ->
  EndDate ->
  m ()
goOnce ground s e = do
  info $ "Starting search for " <> ground.name
  campsites <-
    availableCampsites ground.campsitePredicate ground.dayPredicate
      <$> getCampgroundAvailability ground s e
  if null campsites
    then info $ "Found no availability for " <> ground.name
    else notifyAvailability ground campsites
