module Recreation.Class where

import Recreation.Types

class RecreationClient m where
  getCampgroundAvailability :: CampgroundId -> m [Campsite]

class Notifier m where
  notifyAvailability :: [Campsite] -> m ()
