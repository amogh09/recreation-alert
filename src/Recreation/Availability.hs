{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TupleSections #-}

module Recreation.Availability (go, Config (Config), Env (Env)) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (MonadReader (ask), ReaderT, withReaderT)
import Data.Aeson (FromJSON)
import GHC.Generics (Generic)
import Recreation.Client (fetchCampgroundForRange)
import Recreation.Predicate (availableCampsites)
import Recreation.PushbulletNotifier (ApiToken, notifyAvailability)
import Recreation.Types (Campground (..))
import System.Log.Logger (Logger, Priority (INFO), logL)
import Text.Printf (printf)

data Config = Config {pushBulletToken :: ApiToken}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON)

data Env = Env
  { logger :: Logger,
    config :: Config
  }

go :: (MonadIO m) => [Campground] -> ReaderT Env m ()
go = mapM_ (\c -> withReaderT (,c) findAvailability)

findAvailability :: (MonadIO m) => ReaderT (Env, Campground) m ()
findAvailability = do
  (env, ground) <- ask
  info "Starting search"
  campsites <-
    availableCampsites ground.campsitePredicate ground.dayPredicate
      <$> liftIO (fetchCampgroundForRange ground)
  if null campsites
    then info $ "Found no availability for " <> ground.name
    else do
      info $ "Found available campsites: " <> show campsites
      liftIO $ notifyAvailability env.config.pushBulletToken ground campsites

info :: (MonadIO m) => String -> ReaderT (Env, Campground) m ()
info msg = do
  (env, c) <- ask
  liftIO $ logL env.logger INFO $ printf "%s: %s" (show c) msg
