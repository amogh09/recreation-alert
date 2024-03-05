module Recreation.Predicate where

import Data.Functor.Contravariant (Predicate (Predicate))
import Data.Time (Day, DayOfWeek, dayOfWeek)
import qualified Recreation.Types.CampgroundSearch as CampgroundSearch
import Recreation.Types.Campsite (Campsite)
import qualified Recreation.Types.Campsite as Campsite

dayFrom :: Day -> Predicate Day
dayFrom d = Predicate (>= d)

dayUntil :: Day -> Predicate Day
dayUntil d = Predicate (<= d)

daysBetween :: CampgroundSearch.StartDate -> CampgroundSearch.EndDate -> Predicate Day
daysBetween s e = dayFrom s <> dayUntil e

dayOfWeekIn :: [DayOfWeek] -> Predicate Day
dayOfWeekIn ds = Predicate $ (`elem` ds) . dayOfWeek

siteIn :: [Campsite.Site] -> Predicate Campsite
siteIn sites = Predicate $ (`elem` sites) . Campsite.site

alwaysTrue :: Predicate a
alwaysTrue = Predicate $ const True

anyAvailableDayMatching :: Predicate Day -> Predicate Campsite
anyAvailableDayMatching (Predicate dp) = Predicate $ any dp . Campsite.availableDays
