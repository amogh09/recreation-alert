module MyLib (someFunc) where

import Control.Monad (unless)
import Data.Functor.Contravariant (Predicate (Predicate))
import Data.Time (Day, DayOfWeek, dayOfWeek)
import Recreation.Class
import Recreation.Types

someFunc :: IO ()
someFunc = putStrLn "Hello Haskell"

go ::
  (Monad m, RecreationClient m, Notifier m) =>
  Predicate Campsite ->
  Predicate Day ->
  CampgroundId ->
  m ()
go cp dp cid = do
  campsites <- availableCampsites cp dp <$> getCampgroundAvailability cid
  unless (null campsites) $ notifyAvailability campsites

availableCampsites ::
  Predicate Campsite -> Predicate Day -> [Campsite] -> [Campsite]
availableCampsites (Predicate cp) (Predicate dp) =
  filter (not . null . availabilities)
    . map (mapAvailabilities $ filter (dp . fst))
    . filter cp
