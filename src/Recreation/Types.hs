module Recreation.Types where

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

data Campground = Campground {id :: !String, name :: !String}

data Campsite = Campsite
  { campsiteId :: !String,
    site :: !Site,
    availabilities :: !Availabilities
  }
  deriving (Show, Eq)

mapAvailabilities :: (Availabilities -> Availabilities) -> Campsite -> Campsite
mapAvailabilities f c = c {availabilities = f (availabilities c)}
