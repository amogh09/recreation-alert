module Recreation.Predicate (mkDayPredicate, siteIn, availableCampsites) where

import Control.Lens (over, view)
import Data.Functor.Contravariant (Predicate (Predicate))
import Data.Time (Day, DayOfWeek, dayOfWeek)
import Recreation.Types (Campsite, EndDate, Site, StartDate, availabilities, isAvailable, site)

dayFrom :: Day -> Predicate Day
dayFrom d = Predicate (>= d)

dayUntil :: Day -> Predicate Day
dayUntil d = Predicate (<= d)

dayOfWeekIn :: [DayOfWeek] -> Predicate Day
dayOfWeekIn ds = Predicate $ (`elem` ds) . dayOfWeek

siteIn :: [Site] -> Predicate Campsite
siteIn sites = Predicate $ (`elem` sites) . view site

mkDayPredicate :: StartDate -> EndDate -> [DayOfWeek] -> Predicate Day
mkDayPredicate startDate endDate daysOfWeek =
  mconcat [dayFrom startDate, dayUntil endDate, dayOfWeekIn daysOfWeek]

availableCampsites ::
  Predicate Campsite -> Predicate Day -> [Campsite] -> [Campsite]
availableCampsites (Predicate cp) (Predicate dp) =
  filter (not . null . view availabilities)
    . map (over availabilities $ filter (isAvailable . snd) . filter (dp . fst))
    . filter cp
