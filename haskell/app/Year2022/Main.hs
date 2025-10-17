module Year2022.Main (main) where

import AdventOfCode
import qualified Year2022.Day01 as Day01
import qualified Year2022.Day02 as Day02
import qualified Year2022.Day03 as Day03
import qualified Year2022.Day04 as Day04
import qualified Year2022.Day05 as Day05


$(makePrintResultForDay 2022 [1..5])

main :: IO ()
main = do
  options <- execParser opts
  printResultForDay $ day options
