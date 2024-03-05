module Recreation.PushbulletNotifier (notifyAvailability, ApiToken) where

import Control.Exception (Exception, Handler (Handler), catches, throw)
import Control.Monad ((<=<))
import Control.Monad.Catch (MonadThrow)
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

newtype NotifyException e = NotifyException e

instance Show e => Show (NotifyException e) where
  show (NotifyException e) = "Failed to send notification: " <> show e

instance (Exception e) => Exception (NotifyException e)

notifyAvailability :: ApiToken -> CampgroundSearch -> [Campsite] -> IO ()
notifyAvailability apiToken c cs =
  (createPush apiToken . Push.mkPush . Message.availabilityMsg c $ cs) `catches` [Handler h]
  where
    h :: HttpException -> IO ()
    h = throw . NotifyException

createPush :: ApiToken -> Push -> IO ()
createPush apiToken = void . httpBS <=< pushReq apiToken

pushReq :: (MonadThrow m) => ApiToken -> Push -> m Request
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
