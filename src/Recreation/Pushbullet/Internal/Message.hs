module Recreation.Pushbullet.Internal.Message where

import Recreation.Types.CampgroundSearch (CampgroundSearch)
import qualified Recreation.Types.CampgroundSearch as CampgroundSearch
import Recreation.Types.Campsite (Campsite)
import qualified Recreation.Types.Campsite as Campsite

data Message = Message
  { title :: !String,
    body :: !String
  }

availabilityMsg :: CampgroundSearch -> [Campsite] -> Message
availabilityMsg c cs =
  Message
    (show (length cs) <> " campsites available for " <> CampgroundSearch.name c)
    ("First 10 shown below\n" <> unlines (campsiteMsg <$> take 10 cs))

campsiteMsg :: Campsite -> String
campsiteMsg c = Campsite.site c <> " - " <> show (show . fst <$> Campsite.availabilities c)
