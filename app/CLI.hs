module CLI (opts) where

import Args (Args (Args))
import Options.Applicative

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

opts :: ParserInfo Args
opts =
  info
    (args <**> helper)
    ( fullDesc
        <> progDesc "Recreation.gov campsite search"
        <> header "recreation-alert"
    )
