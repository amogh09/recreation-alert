{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Recreation.Availability (go, Config (Config), Env (Env)) where

import Data.Aeson (FromJSON)
import GHC.Generics (Generic)
import Recreation.Client (fetchCampgroundForRange)
import Recreation.Predicate (availableCampsites)
import Recreation.PushbulletNotifier (ApiToken, notifyAvailability)
import Recreation.Types (Campground (..), EndDate, StartDate)
import System.Log.Logger (Logger, Priority (INFO), logL)

data Config = Config {pushBulletToken :: ApiToken}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON)

data Env = Env
  { logger :: Logger,
    config :: Config
  }

info :: Logger -> String -> IO ()
info l = logL l INFO

go :: Env -> [Campground] -> StartDate -> EndDate -> IO ()
go env cs s e = mapM_ goOnce cs
  where
    goOnce ground = do
      info env.logger ("Starting search for " <> name ground)
      campsites <-
        availableCampsites ground.campsitePredicate ground.dayPredicate
          <$> fetchCampgroundForRange ground s e
      if null campsites
        then info env.logger $ "Found no availability for " <> ground.name
        else notifyAvailability env.config.pushBulletToken ground campsites
