module Main where

import qualified Args
import CLI (opts)
import Data.List ((\\))
import Env (loadEnv)
import Options.Applicative (execParser)
import Recreation.Availability (findAvailabilities, mkCampgroundSearch)
import Recreation.Predicate (alwaysTrue, siteIn)
import Recreation.Types.CampgroundSearch (CampgroundSearch)
import qualified Recreation.Types.CampgroundSearch as CampgroundSearch
import Text.Printf (printf)

cougarRock :: CampgroundSearch.StartDate -> CampgroundSearch.EndDate -> CampgroundSearch
cougarRock s e = mkCampgroundSearch "232466" "Cougar Rock" s e alwaysTrue

kalaloch :: CampgroundSearch.StartDate -> CampgroundSearch.EndDate -> CampgroundSearch
kalaloch s e = do
  let sitePred = siteIn [printf "A%03d" i | i <- [11 .. 28 :: Int] \\ [24]]
  mkCampgroundSearch "232464" "Kalaloch" s e sitePred

ohanapecosh :: CampgroundSearch.StartDate -> CampgroundSearch.EndDate -> CampgroundSearch
ohanapecosh s e = mkCampgroundSearch "232465" "Ohanapecosh Campground" s e alwaysTrue

newHalem :: CampgroundSearch.StartDate -> CampgroundSearch.EndDate -> CampgroundSearch
newHalem s e = mkCampgroundSearch "234060" "Newhalem Campground" s e alwaysTrue

devilsGarden :: CampgroundSearch.StartDate -> CampgroundSearch.EndDate -> CampgroundSearch
devilsGarden s e = mkCampgroundSearch "234059" "Devil's Garden Campground" s e alwaysTrue

main :: IO ()
main = do
  args <- execParser opts
  env <- loadEnv
  findAvailabilities env [devilsGarden (Args.startDate args) (Args.endDate args)]
