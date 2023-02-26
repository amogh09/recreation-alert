module Env (Env, loadEnv) where

import Control.Monad.Reader (ReaderT, asks, liftIO)
import Data.Aeson (FromJSON, eitherDecodeStrict)
import Data.Bifunctor (first)
import qualified Data.ByteString as BS
import GHC.Generics (Generic)
import Recreation.Adapter.HttpClient (fetchCampgroundForRange)
import qualified Recreation.Adapter.PushbulletNotifier as PushBullet
import Recreation.Usecase.Class
  ( Notifier,
    RecreationClient,
    Trace,
    getCampgroundAvailability,
    info,
    notifyAvailability,
  )
import System.FilePath ((</>))
import System.IO (stdout)
import System.Log.Formatter (LogFormatter, simpleLogFormatter)
import System.Log.Handler (setFormatter)
import System.Log.Handler.Simple (fileHandler, streamHandler)
import System.Log.Logger
  ( Logger,
    Priority (INFO),
    getRootLogger,
    logL,
    setHandlers,
    setLevel,
  )
import UnliftIO (MonadIO, fromEither, stringException)
import UnliftIO.Directory (getHomeDirectory)

data Env = Env
  { config :: !Config,
    logger :: Logger
  }

newtype Config = Config {pushBulletToken :: String}
  deriving (Show, Generic)

instance FromJSON Config

instance RecreationClient (ReaderT Env IO) where
  getCampgroundAvailability cid s e =
    liftIO $ fetchCampgroundForRange cid s e

instance Notifier (ReaderT Env IO) where
  notifyAvailability c cs = do
    token <- asks (pushBulletToken . config)
    liftIO . PushBullet.notifyAvailability token c $ cs

instance Trace (ReaderT Env IO) where
  info msg = asks logger >>= \l -> liftIO (logL l INFO msg)

defaultConfigPath :: MonadIO m => m FilePath
defaultConfigPath =
  (</> ".config/recreation-alert.json") <$> getHomeDirectory

loadConfig :: MonadIO m => m Config
loadConfig =
  defaultConfigPath
    >>= liftIO . BS.readFile
    >>= fromEither . first stringException . eitherDecodeStrict

loadEnv :: MonadIO m => m Env
loadEnv = Env <$> loadConfig <*> defaultLogger

defaultLogFilePath :: MonadIO m => m FilePath
defaultLogFilePath =
  liftIO (fmap (</> "recreation-alert.log") getHomeDirectory)

defaultLogFormat :: LogFormatter a
defaultLogFormat = simpleLogFormatter "[$utcTime : $prio] $msg"

defaultLogger :: MonadIO m => m Logger
defaultLogger = liftIO $ do
  path <- defaultLogFilePath
  setHandlers
    <$> sequence
      [ flip setFormatter defaultLogFormat <$> streamHandler stdout INFO,
        flip setFormatter defaultLogFormat <$> fileHandler path INFO
      ]
    <*> (setLevel INFO <$> getRootLogger)
