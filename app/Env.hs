{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}
module Env (Env (Env), mkEnvFromConfig) where

import Control.Monad.Reader (ReaderT, asks, liftIO)
import Data.Aeson
import Data.Bifunctor (first)
import qualified Data.ByteString as BS
import GHC.Generics (Generic)
import qualified Pushbullet.Notifier as PushBullet
import Recreation.Class (Notifier, RecreationClient, getCampgroundAvailability, notifyAvailability, notifyNoAvailability)
import Recreation.Client (fetchCampgroundForRange)
import System.FilePath ((</>))
import UnliftIO (MonadIO, fromEither, stringException)
import UnliftIO.Directory (getHomeDirectory)

data Env = Env {pushBulletToken :: !String}
  deriving (Show, Generic)

instance FromJSON Env

instance RecreationClient (ReaderT Env IO) where
  getCampgroundAvailability cid s e =
    liftIO $ fetchCampgroundForRange cid s e

instance Notifier (ReaderT Env IO) where
  notifyAvailability c cs = do
    liftIO $ putStrLn "Availability found!"
    token <- asks pushBulletToken
    liftIO . PushBullet.notifyAvailability token c $ cs

  notifyNoAvailability = liftIO $ putStrLn "No availability found :("

defaultConfigPath :: MonadIO m => m FilePath
defaultConfigPath =
  (</> ".config/recreation-alert.json") <$> getHomeDirectory

mkEnvFromConfig :: MonadIO m => m Env
mkEnvFromConfig =
  defaultConfigPath
    >>= liftIO . BS.readFile
    >>= fromEither . first stringException . eitherDecodeStrict
