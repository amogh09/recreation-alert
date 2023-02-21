module Recreation.Usecase.Class where

import Recreation.Core.Types
  ( Campground,
    Campsite,
    EndDate,
    StartDate,
  )

class RecreationClient m where
  getCampgroundAvailability ::
    Campground -> StartDate -> EndDate -> m [Campsite]

class Notifier m where
  notifyAvailability :: Campground -> [Campsite] -> m ()
  notifyNoAvailability :: Campground -> m ()
