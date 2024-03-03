module Main where

import CLI (Args (..), opts)
import Control.Monad.Reader (ReaderT (runReaderT))
import Data.List ((\\))
import Env (loadEnv)
import Options.Applicative (execParser)
import Recreation.Availability (go)
import Recreation.Predicate (alwaysTrue, siteIn)
import Recreation.Types
import Text.Printf (printf)

cougarRock :: StartDate -> EndDate -> Campground
cougarRock s e = Campground "232466" "Cougar Rock" s e alwaysTrue alwaysTrue

kalaloch :: StartDate -> EndDate -> Campground
kalaloch s e =
  Campground
    { name = "Kalaloch",
      id = "232464",
      dayPredicate = alwaysTrue,
      campsitePredicate = siteIn [printf "A%03d" i | i <- [11 .. 28 :: Int] \\ [24]],
      startDate = s,
      endDate = e
    }

ohanapecosh :: StartDate -> EndDate -> Campground
ohanapecosh s e =
  Campground
    { name = "Ohanapecosh Campground",
      id = "232465",
      dayPredicate = alwaysTrue,
      campsitePredicate = alwaysTrue,
      startDate = s,
      endDate = e
    }

newHalem :: StartDate -> EndDate -> Campground
newHalem s e =
  Campground
    { name = "Newhalem Campground",
      id = "234060",
      dayPredicate = alwaysTrue,
      campsitePredicate = alwaysTrue,
      startDate = s,
      endDate = e
    }

devilsGarden :: StartDate -> EndDate -> Campground
devilsGarden s e =
  Campground
    { name = "Devil's Garden Campground",
      id = "234059",
      dayPredicate = alwaysTrue,
      campsitePredicate = alwaysTrue,
      startDate = s,
      endDate = e
    }

main :: IO ()
main = do
  args <- execParser opts
  env <- loadEnv
  runReaderT (go [devilsGarden args.startDate args.endDate]) env
