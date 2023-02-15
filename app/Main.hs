module Main where

import CLI (Args (..), opts)
import Control.Monad.Reader (ReaderT (runReaderT))
import Data.Functor.Contravariant (Predicate (Predicate))
import Data.Time.Calendar
import Env (loadEnv)
import MyLib (go)
import Options.Applicative (execParser)
import Recreation.Predicate (mkDayPredicate, siteIn)
import Recreation.Types (Campground (Campground))
import Text.Printf (printf)

main :: IO ()
main = do
  args <- execParser opts
  let dayPred =
        mkDayPredicate args.startDate args.endDate [Friday .. Sunday]
  env <- loadEnv
  runReaderT
    ( go
        [ Campground
            "232466"
            "Cougar Rock"
            (Predicate $ const True)
            dayPred,
          Campground
            "232464"
            "Kalaloch"
            (siteIn [printf "A%03d" i | i <- [11 .. 28 :: Int]])
            dayPred
        ]
        args.startDate
        args.endDate
    )
    env
