module Args where

import qualified Recreation.Types.CampgroundSearch as CampgroundSearch

data Args = Args
  { startDate :: CampgroundSearch.StartDate,
    endDate :: CampgroundSearch.EndDate
  }
