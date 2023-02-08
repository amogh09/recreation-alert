module Recreation.Predicate () where

import Data.Functor.Contravariant (Predicate (Predicate))
import Data.Time (Day, DayOfWeek, dayOfWeek)
import Recreation.Types (Campsite (site), Site)

dayFrom :: Day -> Predicate Day
dayFrom d = Predicate (>= d)

dayUntil :: Day -> Predicate Day
dayUntil d = Predicate (<= d)

dayOfWeekIn :: [DayOfWeek] -> Predicate Day
dayOfWeekIn ds = Predicate $ (`elem` ds) . dayOfWeek

siteIn :: [Site] -> Predicate Campsite
siteIn sites = Predicate $ (`elem` sites) . site

mkDayPredicate :: Day -> Day -> [DayOfWeek] -> Predicate Day
mkDayPredicate startDate endDate daysOfWeek =
  mconcat [dayFrom startDate, dayUntil endDate, dayOfWeekIn daysOfWeek]
