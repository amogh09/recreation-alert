{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Recreation.Availability
  ( findAvailabilities,
    Config (Config),
    Env (Env),
    mkCampgroundSearch,
  )
where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (MonadReader (ask), ReaderT (runReaderT))
import Data.Aeson (FromJSON)
import Data.Functor.Contravariant (Predicate (getPredicate))
import GHC.Generics (Generic)
import Recreation.Client (fetchCampgroundForRange)
import Recreation.Predicate (anyAvailableDayMatching, daysBetween)
import Recreation.PushbulletNotifier (ApiToken, notifyAvailability)
import Recreation.Types
import System.Log.Logger (Logger, Priority (INFO), logL)
import Text.Printf (printf)

data Config = Config {pushBulletToken :: ApiToken}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON)

data Env = Env
  { logger :: Logger,
    config :: Config
  }

mkCampgroundSearch ::
  CampgroundId -> CampgroundName -> StartDate -> EndDate -> Predicate Campsite -> CampgroundSearch
mkCampgroundSearch cid cname s e cp =
  CampgroundSearch
    { id = cid,
      name = cname,
      startDate = s,
      endDate = e,
      campsitePredicate = anyAvailableDayMatching (daysBetween s e) <> cp
    }

findAvailabilities :: Env -> [CampgroundSearch] -> IO ()
findAvailabilities env = mapM_ (\c -> runReaderT findAvailability (env, c))

findAvailability :: (MonadIO m, MonadReader (Env, CampgroundSearch) m) => m ()
findAvailability = do
  (env, cg) <- ask
  info "Starting search"
  campsites <- filter (getPredicate $ campsitePredicate cg) <$> liftIO (fetchCampgroundForRange cg)
  if null campsites
    then info $ printf "Found no availabilty for %s" cg.name
    else do
      info $ printf "Found available campsites: %s" (show campsites)
      liftIO $ notifyAvailability (pushBulletToken $ config env) cg campsites

info :: (MonadIO m, MonadReader (Env, CampgroundSearch) m) => String -> m ()
info msg = do
  (env, c) <- ask
  liftIO $ logL env.logger INFO $ printf "%s: %s" (show c) msg
