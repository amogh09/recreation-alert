module Recreation.Pushbullet.Internal.Push where

import Data.Aeson
import Recreation.Pushbullet.Internal.Message (Message)
import qualified Recreation.Pushbullet.Internal.Message as Message

data Push = Push
  { title :: !String,
    body :: !String,
    pushType :: !String
  }
  deriving (Show)

instance ToJSON Push where
  toJSON push =
    object
      [ "title" .= title push,
        "body" .= body push,
        "type" .= pushType push
      ]

mkPush :: Message -> Push
mkPush msg =
  Push
    { title = Message.title msg,
      body = Message.body msg,
      pushType = "note"
    }
