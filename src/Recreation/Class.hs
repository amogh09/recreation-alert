module Recreation.Class where

import Recreation.Types
  ( CampgroundId,
    Campsite,
    EndDate,
    StartDate,
  )

class RecreationClient m where
  getCampgroundAvailability :: CampgroundId -> StartDate -> EndDate -> m [Campsite]

class Notifier m where
  notifyAvailability :: [Campsite] -> m ()
  notifyNoAvailability :: m ()
