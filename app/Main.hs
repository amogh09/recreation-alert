module Main where

import CLI (Args (..), opts)
import Control.Monad.Reader (ReaderT (runReaderT))
import Data.Time.Calendar
import Env (mkEnvFromConfig)
import MyLib (go)
import Options.Applicative (execParser)
import Recreation.Predicate (mkDayPredicate, siteIn)
import Recreation.Types (Campground (Campground))
import Text.Printf (printf)

main :: IO ()
main = do
  args <- execParser opts
  let campsitePred = siteIn [printf "A%03d" i | i <- [11 .. 28 :: Int]]
      dayPred =
        mkDayPredicate args.startDate args.endDate [Friday .. Sunday]
  env <- mkEnvFromConfig
  runReaderT
    ( go
        campsitePred
        dayPred
        (Campground args.campgroundId args.campgroundName)
        args.startDate
        args.endDate
    )
    env
