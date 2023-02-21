module Recreation.Usecase.Availability (go) where

import Control.Monad (forM_)
import Recreation.Core.Predicate (availableCampsites)
import Recreation.Core.Types
  ( Campground (..),
    EndDate,
    StartDate,
  )
import Recreation.Usecase.Class (Notifier (..), RecreationClient (..))
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
go cs s e = forM_ cs $ \c -> goOnce c s e

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
