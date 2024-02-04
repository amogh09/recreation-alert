module Recreation.Usecase.Availability (go) where

import Recreation.Core.Predicate (availableCampsites)
import Recreation.Core.Types (Campground (..), EndDate, StartDate)
import Recreation.Usecase.Class (Notifier (..), RecreationClient (..), Trace, info)
import UnliftIO (MonadIO)

go ::
  (RecreationClient m, Notifier m, MonadIO m, Trace m) =>
  [Campground] ->
  StartDate ->
  EndDate ->
  m ()
go cs s e = mapM_ goOnce cs
  where
    goOnce ground = do
      info $ "Starting search for " <> ground.name
      campsites <-
        availableCampsites ground.campsitePredicate ground.dayPredicate
          <$> getCampgroundAvailability ground s e
      if null campsites
        then info $ "Found no availability for " <> ground.name
        else notifyAvailability ground campsites
