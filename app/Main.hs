module Main where

import CLI (Args (..), opts)
import Control.Monad.Reader (ReaderT (runReaderT))
import Data.List ((\\))
import Data.Time.Calendar
import Env (loadEnv)
import Options.Applicative (execParser)
import Recreation.Core.Predicate (mkDayPredicate, siteIn)
import Recreation.Core.Types (Campground (Campground))
import Recreation.Usecase.Availability (go)
import Text.Printf (printf)

main :: IO ()
main = do
  args <- execParser opts
  let dayPred =
        mkDayPredicate args.startDate args.endDate [Friday .. Sunday]
  env <- loadEnv
  runReaderT
    ( go
        [ -- Campground
          --   "232466"
          --   "Cougar Rock"
          --   (Predicate $ const True)
          --   dayPred,
          Campground
            "232464"
            "Kalaloch"
            (siteIn [printf "A%03d" i | i <- [11 .. 28 :: Int] \\ [24]])
            dayPred
            -- Campground
            --   "232465"
            --   "Ohanapecosh Campground"
            --   (Predicate $ const True)
            --   dayPred,
            -- Campground
            --   "234060"
            --   "Newhalem Campground"
            --   (Predicate $ const True)
            --   dayPred
        ]
        args.startDate
        args.endDate
    )
    env
