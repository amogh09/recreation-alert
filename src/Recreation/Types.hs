module Recreation.Types where

import Data.Time (Day)

data Availability = Available | NotAvailable
  deriving (Show)

type CampgroundId = String

type Availabilities = [(Day, Availability)]

type Site = String

data Campsite = Campsite
  { campsiteId :: !String,
    site :: !Site,
    availabilities :: !Availabilities
  }
  deriving (Show)

mapAvailabilities :: (Availabilities -> Availabilities) -> Campsite -> Campsite
mapAvailabilities f c = c {availabilities = f (availabilities c)}
