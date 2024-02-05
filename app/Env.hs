module Env (loadEnv) where

import Data.Aeson (eitherDecodeStrict)
import qualified Data.ByteString as BS
import Recreation.Availability (Config, Env (Env))
import System.Directory (getHomeDirectory)
import System.FilePath ((</>))
import System.IO (stdout)
import System.Log.Formatter (LogFormatter, simpleLogFormatter)
import System.Log.Handler (setFormatter)
import System.Log.Handler.Simple (fileHandler, streamHandler)
import System.Log.Logger (Logger, Priority (INFO), getRootLogger, setHandlers, setLevel)

defaultConfigPath :: IO FilePath
defaultConfigPath =
  (</> ".config/recreation-alert.json") <$> getHomeDirectory

loadConfig :: IO Config
loadConfig =
  defaultConfigPath
    >>= BS.readFile
    >>= either fail pure . eitherDecodeStrict

loadEnv :: IO Env
loadEnv = Env <$> defaultLogger <*> loadConfig

defaultLogFilePath :: IO FilePath
defaultLogFilePath = fmap (</> "recreation-alert.log") getHomeDirectory

defaultLogFormat :: LogFormatter a
defaultLogFormat = simpleLogFormatter "[$utcTime : $prio] $msg"

defaultLogger :: IO Logger
defaultLogger = do
  path <- defaultLogFilePath
  setHandlers
    <$> sequence
      [ flip setFormatter defaultLogFormat <$> streamHandler stdout INFO,
        flip setFormatter defaultLogFormat <$> fileHandler path INFO
      ]
    <*> (setLevel INFO <$> getRootLogger)
