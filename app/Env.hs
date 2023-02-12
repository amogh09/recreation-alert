module Env (Env (Env)) where

import Control.Monad.Reader (ReaderT, liftIO)
import Recreation.Class (Notifier, RecreationClient, getCampgroundAvailability, notifyAvailability, notifyNoAvailability)
import Recreation.Client (fetchCampgroundForRange)

data Env = Env

instance RecreationClient (ReaderT Env IO) where
  getCampgroundAvailability cid s e = liftIO $ fetchCampgroundForRange cid s e

instance Notifier (ReaderT Env IO) where
  notifyAvailability cs = liftIO $ do
    putStrLn "Found availability!"
    mapM_ print cs

  notifyNoAvailability = liftIO $ putStrLn "No availability found :("
