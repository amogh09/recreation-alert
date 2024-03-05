module Recreation.Predicate where

import Control.Lens (view)
import Data.Functor.Contravariant (Predicate (Predicate))
import Data.Time (Day, DayOfWeek, dayOfWeek)
import Recreation.Types

dayFrom :: Day -> Predicate Day
dayFrom d = Predicate (>= d)

dayUntil :: Day -> Predicate Day
dayUntil d = Predicate (<= d)

daysBetween :: StartDate -> EndDate -> Predicate Day
daysBetween s e = dayFrom s <> dayUntil e

dayOfWeekIn :: [DayOfWeek] -> Predicate Day
dayOfWeekIn ds = Predicate $ (`elem` ds) . dayOfWeek

siteIn :: [Site] -> Predicate Campsite
siteIn sites = Predicate $ (`elem` sites) . view site

alwaysTrue :: Predicate a
alwaysTrue = Predicate $ const True

anyAvailableDayMatching :: Predicate Day -> Predicate Campsite
anyAvailableDayMatching (Predicate dp) = Predicate $ any dp . campsiteAvailableDays
