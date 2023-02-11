module Recreation.Class where

import Data.Time (Day)
import Recreation.Types

type StartDate = Day

type EndDate = Day

class RecreationClient m where
  getCampgroundAvailability :: CampgroundId -> m [Campsite]

class Notifier m where
  notifyAvailability :: [Campsite] -> m ()
