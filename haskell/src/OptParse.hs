module OptParse (
  Options(..)
, optionsParser
, opts
, execParser
) where

import Options.Applicative

data Options = Options
  { day :: String
  } deriving (Eq, Show)

optionsParser :: Parser Options
optionsParser = Options
  <$> strOption (long "day" <> short 'd' <> help "day")

opts = info (optionsParser <**> helper)
  ( fullDesc
  <> progDesc "Parse input and output file names"
  <> header "my-program - a test for optparse-applicative" )
