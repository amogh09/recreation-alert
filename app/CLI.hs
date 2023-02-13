module CLI (Args (..), opts) where

import Options.Applicative
import Recreation.Types

data Args = Args
  { startDate :: StartDate,
    endDate :: EndDate,
    campgroundId :: String,
    campgroundName :: String
  }

args :: Parser Args
args =
  Args
    <$> option
      auto
      ( long "start-date"
          <> short 's'
          <> metavar "YYYY-MM-DD"
          <> help "Start date for the campsite search"
      )
    <*> option
      auto
      ( long "end-date"
          <> short 'e'
          <> metavar "YYYY-MM-DD"
          <> help "End date for the campsite search"
      )
    <*> strOption
      ( long "campground-id"
          <> short 'c'
          <> metavar "STRING"
          <> help "Campground ID for the campsite search"
      )
    <*> strOption
      ( long "campground-name"
          <> short 'n'
          <> metavar "STRING"
          <> help "Campground name for the campsite search"
      )

opts :: ParserInfo Args
opts =
  info
    (args <**> helper)
    (fullDesc <> progDesc "Recreation.gov campsite search" <> header "recreation-alert")
