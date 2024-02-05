module Recreation.PushbulletNotifier (notifyAvailability, ApiToken) where

import Control.Lens (view)
import Control.Monad ((<=<))
import Control.Monad.Catch (MonadThrow)
import Data.Aeson
import qualified Data.ByteString.Char8 as BC8
import Data.Functor (void)
import Network.HTTP.Client.Conduit (parseRequest, setRequestCheckStatus)
import Network.HTTP.Simple (Request, httpBS, setRequestBodyJSON, setRequestHeaders, setRequestMethod)
import Network.HTTP.Types (hContentType)
import Recreation.Types

type ApiToken = String

data Push = Push
  { title :: !String,
    body :: !String,
    pushType :: !String
  }
  deriving (Show)

data Message = Message
  { title :: !String,
    body :: !String
  }

instance ToJSON Push where
  toJSON push =
    object
      [ "title" .= push.title,
        "body" .= push.body,
        "type" .= push.pushType
      ]

mkPush :: Message -> Push
mkPush msg =
  Push
    { title = msg.title,
      body = msg.body,
      pushType = "note"
    }

notifyAvailability :: ApiToken -> Campground -> [Campsite] -> IO ()
notifyAvailability apiToken c =
  createPush apiToken . mkPush . availabilityMsg c

availabilityMsg :: Campground -> [Campsite] -> Message
availabilityMsg c cs =
  Message
    (show (length cs) <> " campsites available for " <> c.name)
    ("First 10 shown below\n" <> unlines (campsiteMsg <$> take 10 cs))

campsiteMsg :: Campsite -> String
campsiteMsg c =
  view site c
    <> " - "
    <> show (show . fst <$> view availabilities c)

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
