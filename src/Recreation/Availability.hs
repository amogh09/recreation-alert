module Recreation.Availability
  ( findAvailabilities,
    Config (Config),
    Env (Env),
    mkCampgroundSearch,
  )
where

import Control.Monad (forM_)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (MonadReader (ask), ReaderT (runReaderT))
import Data.Aeson (FromJSON)
import Data.Functor.Contravariant (Predicate (getPredicate))
import GHC.Generics (Generic)
import Recreation.Client (fetchCampgroundForRange)
import Recreation.Predicate (anyAvailableDayMatching, daysBetween)
import Recreation.PushbulletNotifier (ApiToken, notifyAvailability)
import Recreation.Types.CampgroundSearch (CampgroundSearch (CampgroundSearch))
import qualified Recreation.Types.CampgroundSearch as CSearch
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
  CSearch.CampgroundId ->
  CSearch.CampgroundName ->
  CSearch.StartDate ->
  CSearch.EndDate ->
  Predicate Campsite ->
  CampgroundSearch
mkCampgroundSearch cid cname s e cp =
  CampgroundSearch cid cname s e (anyAvailableDayMatching (daysBetween s e) <> cp)

findAvailabilities :: Env -> [CampgroundSearch] -> IO ()
findAvailabilities env = mapM_ (\c -> runReaderT findAvailability (env, c))

type AvailMonad m = (MonadIO m, MonadReader (Env, CampgroundSearch) m, MonadCatch m)

findAvailability :: AvailMonad m => m ()
findAvailability = do
  (env, cg) <- ask
  info "Starting search"
  campsites <- filter (getPredicate $ CSearch.campsitePredicate cg) <$> fetchCampgroundForRange cg
  if null campsites
    then info $ printf "Found no availabilty for %s" (CSearch.name cg)
    else do
      info $ printf "Found available campsites"
      forM_ campsites $ \c -> do
        info $ printf "Site %s - %s" (Campsite.site c) (show $ Campsite.availableDays c)
      notifyAvailability (pushBulletToken $ config env) cg campsites

info :: AvailMonad m => String -> m ()
info msg = do
  (env, c) <- ask
  liftIO $ logL (logger env) INFO $ printf "%s: %s" (show c) msg
