module Recreation.Types.Campsite where

import Data.Time (Day)

data Availability = Available | NotAvailable
  deriving (Show, Eq)

isAvailable :: Availability -> Bool
isAvailable Available = True
isAvailable _ = False

type Availabilities = [(Day, Availability)]

type Site = String

data Campsite = Campsite
  { campsiteId :: !String,
    site :: !Site,
    availabilities :: !Availabilities
  }
  deriving (Show, Eq)

-- Returns all available days for the campsite.
availableDays :: Campsite -> [Day]
availableDays = fmap fst . filter (isAvailable . snd) . availabilities
