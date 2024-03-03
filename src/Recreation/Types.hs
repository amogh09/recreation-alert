module Recreation.Types where

import Control.Exception (Exception)
import Control.Lens (makeLenses)
import Data.Functor.Contravariant (Predicate)
import Data.Time (Day)
import Text.Printf (printf)

data Availability = Available | NotAvailable
  deriving (Show, Eq)

isAvailable :: Availability -> Bool
isAvailable Available = True
isAvailable _ = False

type Availabilities = [(Day, Availability)]

type Site = String

type StartDate = Day

type EndDate = Day

data Campground = Campground
  { id :: !String,
    name :: !String,
    startDate :: StartDate,
    endDate :: EndDate,
    campsitePredicate :: Predicate Campsite,
    dayPredicate :: Predicate Day
  }

instance Show Campground where
  show c = printf "%s (%s to %s)" c.name (show c.startDate) (show c.endDate)

data Campsite = Campsite
  { _campsiteId :: !String,
    _site :: !Site,
    _availabilities :: !Availabilities
  }
  deriving (Show, Eq)

makeLenses ''Campsite

newtype StringException = StringException String

instance Show StringException where
  show (StringException msg) = msg

instance Exception StringException

stringException :: String -> StringException
stringException = StringException
