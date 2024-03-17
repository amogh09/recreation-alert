module Recreation.PushbulletNotifier (notifyAvailability, ApiToken) where

import Control.Monad ((<=<))
import Control.Monad.Catch (Exception, Handler (Handler), MonadCatch, MonadThrow (throwM), catches)
import Control.Monad.IO.Class (MonadIO)
import qualified Data.ByteString.Char8 as BC8
import Data.Functor (void)
import Network.HTTP.Client.Conduit (HttpException, parseRequest, setRequestCheckStatus)
import Network.HTTP.Simple (Request, httpBS, setRequestBodyJSON, setRequestHeaders, setRequestMethod)
import Network.HTTP.Types (hContentType)
import qualified Recreation.Pushbullet.Internal.Message as Message
import Recreation.Pushbullet.Internal.Push (Push)
import qualified Recreation.Pushbullet.Internal.Push as Push
import Recreation.Types.CampgroundSearch (CampgroundSearch)
import Recreation.Types.Campsite (Campsite)

type ApiToken = String

type PBMonad m = (MonadIO m, MonadThrow m, MonadCatch m)

newtype NotifyException e = NotifyException e

instance Show e => Show (NotifyException e) where
  show (NotifyException e) = "Failed to send notification: " <> show e

instance (Exception e) => Exception (NotifyException e)

notifyAvailability :: PBMonad m => ApiToken -> CampgroundSearch -> [Campsite] -> m ()
notifyAvailability apiToken c cs =
  (createPush apiToken . Push.mkPush . Message.availabilityMsg c $ cs) `catches` [Handler h]
  where
    h :: PBMonad m => HttpException -> m ()
    h = throwM . NotifyException

createPush :: PBMonad m => ApiToken -> Push -> m ()
createPush apiToken = void . httpBS <=< pushReq apiToken

pushReq :: PBMonad m => ApiToken -> Push -> m Request
pushReq apiToken push =
  fmap
    ( setRequestHeaders
        [ (hContentType, "application/json"),
          ("Access-Token", BC8.pack apiToken)
        ]
        . setRequestCheckStatus
        . setRequestBodyJSON push
        . setRequestMethod "POST"
    )
    . parseRequest
    $ "https://api.pushbullet.com/v2/pushes"
