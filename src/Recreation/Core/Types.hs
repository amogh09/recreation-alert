module Recreation.Core.Types where

import Control.Lens (makeLenses)
import Data.Functor.Contravariant (Predicate)
import Data.Time (Day)

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
    campsitePredicate :: Predicate Campsite,
    dayPredicate :: Predicate Day
  }

data Campsite = Campsite
  { _campsiteId :: !String,
    _site :: !Site,
    _availabilities :: !Availabilities
  }
  deriving (Show, Eq)

makeLenses ''Campsite
