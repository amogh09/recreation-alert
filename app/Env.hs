module Env (Env, loadEnv) where

import Control.Monad.Reader (ReaderT, asks, liftIO)
import Data.Aeson
import Data.Bifunctor (first)
import qualified Data.ByteString as BS
import Data.Time (getCurrentTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import GHC.Generics (Generic)
import qualified Pushbullet.Notifier as PushBullet
import Recreation.Class (Notifier, RecreationClient, getCampgroundAvailability, notifyAvailability, notifyNoAvailability)
import Recreation.Client (fetchCampgroundForRange)
import Recreation.Types (Campground (name))
import System.FilePath ((</>))
import System.IO (hPutStrLn)
import UnliftIO (Handle, IOMode (AppendMode), MonadIO, fromEither, hFlush, openFile, stringException)
import UnliftIO.Directory (getHomeDirectory)

data Env = Env
  { config :: !Config,
    logHandle :: Handle
  }

newtype Config = Config {pushBulletToken :: String}
  deriving (Show, Generic)

instance FromJSON Config

instance RecreationClient (ReaderT Env IO) where
  getCampgroundAvailability cid s e =
    liftIO $ fetchCampgroundForRange cid s e

instance Notifier (ReaderT Env IO) where
  notifyAvailability c cs = do
    h <- asks logHandle
    logMsg $ "Availability found for " <> c.name <> "!"
    hFlush h
    token <- asks (pushBulletToken . config)
    liftIO . PushBullet.notifyAvailability token c $ cs

  notifyNoAvailability c = do
    h <- asks logHandle
    logMsg $ "No availability found for " <> c.name <> " :("
    hFlush h

logMsg :: String -> ReaderT Env IO ()
logMsg msg = do
  h <- asks logHandle
  now <- iso8601Show <$> liftIO getCurrentTime
  liftIO $ hPutStrLn h (now <> " " <> msg)
  hFlush h

defaultConfigPath :: MonadIO m => m FilePath
defaultConfigPath =
  (</> ".config/recreation-alert.json") <$> getHomeDirectory

loadConfig :: MonadIO m => m Config
loadConfig =
  defaultConfigPath
    >>= liftIO . BS.readFile
    >>= fromEither . first stringException . eitherDecodeStrict

loadEnv :: MonadIO m => m Env
loadEnv = Env <$> loadConfig <*> initLogHandle

defaultLogFilePath :: MonadIO m => m FilePath
defaultLogFilePath = (</> "recreation-alert.log") <$> getHomeDirectory

initLogHandle :: MonadIO m => m Handle
initLogHandle = defaultLogFilePath >>= flip openFile AppendMode
