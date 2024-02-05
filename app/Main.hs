module Main where

import CLI (Args (..), opts)
import Data.Functor.Contravariant (Predicate (Predicate))
import Data.List ((\\))
import Data.Time.Calendar
import Env (loadEnv)
import Options.Applicative (execParser)
import Recreation.Availability (go)
import Recreation.Predicate (mkDayPredicate, siteIn)
import Recreation.Types (Campground (Campground))
import System.Directory (getHomeDirectory)
import Text.Printf (printf)

cougarRock :: Predicate Day -> Campground
cougarRock = Campground "232466" "Cougar Rock" (Predicate $ const True)

kalaloch :: Predicate Day -> Campground
kalaloch =
  Campground "232464" "Kalaloch" (siteIn [printf "A%03d" i | i <- [11 .. 28 :: Int] \\ [24]])

ohanapecosh :: Predicate Day -> Campground
ohanapecosh = Campground "232465" "Ohanapecosh Campground" (Predicate $ const True)

newHalem :: Predicate Day -> Campground
newHalem = Campground "234060" "Newhalem Campground" (Predicate $ const True)

devilsGarden :: Predicate Day -> Campground
devilsGarden = Campground "234059" "Devil's Garden Campground" (Predicate $ const True)

main :: IO ()
main = do
  args <- execParser opts
  let dayPred = mkDayPredicate args.startDate args.endDate [Monday .. Sunday]
  env <- loadEnv
  go env [c dayPred | c <- [devilsGarden]] args.startDate args.endDate
