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

import Control.Monad (forM_)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (MonadReader (ask), ReaderT (runReaderT))
import Data.Aeson (FromJSON)
import Data.Functor.Contravariant (Predicate (getPredicate))
import GHC.Generics (Generic)
import Recreation.Client (fetchCampgroundForRange)
import Recreation.Predicate (anyAvailableDayMatching, daysBetween)
import Recreation.PushbulletNotifier (ApiToken, notifyAvailability)
import Recreation.Types.CampgroundSearch (CampgroundSearch (CampgroundSearch))
import qualified Recreation.Types.CampgroundSearch as CampgroundSearch
import Recreation.Types.Campsite (Campsite)
import qualified Recreation.Types.Campsite as Campsite
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
  CampgroundSearch.CampgroundId ->
  CampgroundSearch.CampgroundName ->
  CampgroundSearch.StartDate ->
  CampgroundSearch.EndDate ->
  Predicate Campsite ->
  CampgroundSearch
mkCampgroundSearch cid cname s e cp =
  CampgroundSearch cid cname s e (anyAvailableDayMatching (daysBetween s e) <> cp)

findAvailabilities :: Env -> [CampgroundSearch] -> IO ()
findAvailabilities env = mapM_ (\c -> runReaderT findAvailability (env, c))

findAvailability :: (MonadIO m, MonadReader (Env, CampgroundSearch) m) => m ()
findAvailability = do
  (env, cg) <- ask
  info "Starting search"
  campsites <-
    filter (getPredicate $ CampgroundSearch.campsitePredicate cg)
      <$> liftIO (fetchCampgroundForRange cg)
  if null campsites
    then info $ printf "Found no availabilty for %s" cg.name
    else do
      info $ printf "Found available campsites"
      forM_ campsites $ \c -> do
        info $ printf "Site: %s" (Campsite.site c)
        info $ printf "Availabilities: %s" (show $ Campsite.availableDays c)
      liftIO $ notifyAvailability (pushBulletToken $ config env) cg campsites

info :: (MonadIO m, MonadReader (Env, CampgroundSearch) m) => String -> m ()
info msg = do
  (env, c) <- ask
  liftIO $ logL env.logger INFO $ printf "%s: %s" (show c) msg
