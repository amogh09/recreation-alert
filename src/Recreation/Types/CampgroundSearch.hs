module Recreation.Types.CampgroundSearch where

import Data.Functor.Contravariant (Predicate)
import Data.Time (Day)
import Recreation.Types.Campsite (Campsite)
import Text.Printf (printf)

type StartDate = Day

type EndDate = Day

type CampgroundName = String

type CampgroundId = String

data CampgroundSearch = CampgroundSearch
  { id :: !CampgroundId,
    name :: !CampgroundName,
    startDate :: StartDate,
    endDate :: EndDate,
    campsitePredicate :: Predicate Campsite
  }

instance Show CampgroundSearch where
  show c = printf "%s (%s to %s)" (name c) (show $ startDate c) (show $ endDate c)
