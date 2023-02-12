module MyLib (go) where

import Data.Functor.Contravariant (Predicate (Predicate))
import Data.Time (Day)
import Recreation.Class (Notifier (..), RecreationClient (..))
import Recreation.Types
  ( CampgroundId,
    Campsite (availabilities),
    EndDate,
    StartDate,
    isAvailable,
    mapAvailabilities,
  )

go ::
  (Monad m, RecreationClient m, Notifier m) =>
  Predicate Campsite ->
  Predicate Day ->
  CampgroundId ->
  StartDate ->
  EndDate ->
  m ()
go cp dp cid s e = do
  campsites <- availableCampsites cp dp <$> getCampgroundAvailability cid s e
  if null campsites
    then notifyNoAvailability
    else notifyAvailability campsites

availableCampsites ::
  Predicate Campsite -> Predicate Day -> [Campsite] -> [Campsite]
availableCampsites (Predicate cp) (Predicate dp) =
  filter (not . null . availabilities)
    . map (mapAvailabilities $ filter (isAvailable . snd) . filter (dp . fst))
    . filter cp
