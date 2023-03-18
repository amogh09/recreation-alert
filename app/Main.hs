module Main where

import CLI (Args (..), opts)
import Control.Monad.Reader (ReaderT (runReaderT))
import Data.Functor.Contravariant (Predicate (Predicate))
import Data.List ((\\))
import Data.Time.Calendar
import Env (loadEnv)
import Options.Applicative (execParser)
import Recreation.Core.Predicate (mkDayPredicate, siteIn)
import Recreation.Core.Types (Campground (Campground))
import Recreation.Usecase.Availability (go)
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

main :: IO ()
main = do
  args <- execParser opts
  let dayPred =
        mkDayPredicate args.startDate args.endDate [Friday .. Sunday]
  env <- loadEnv
  runReaderT
    ( go
        [c dayPred | c <- []]
        args.startDate
        args.endDate
    )
    env
