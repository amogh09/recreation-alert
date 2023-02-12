module Main where

import CLI (Args (campgroundId, endDate, startDate), opts)
import Control.Monad.Reader (ReaderT (runReaderT))
import Data.Functor.Contravariant (Predicate (Predicate))
import Data.Time.Calendar
import Env (Env (Env))
import MyLib (go)
import Options.Applicative (execParser)
import Recreation.Predicate (mkDayPredicate)

main :: IO ()
main = do
  args <- execParser opts
  let campsitePred = Predicate $ const True
      dayPred = mkDayPredicate args.startDate args.endDate [Monday]
  runReaderT (go campsitePred dayPred args.campgroundId args.startDate args.endDate) Env
