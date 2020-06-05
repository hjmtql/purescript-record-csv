module Test.Overflow where

import Prelude
import Data.Array (replicate)
import Data.List as L
import Record.CSV.Parser (parseCSV)
import Record.CSV.Printer (printCSV)
import Record.CSV.Type (CSVResult)
import Test.Unit (TestSuite, suite, test)

type Log
  = { message :: String
    , temperature :: Number
    }

xs :: L.List Log
xs =
  L.fromFoldable
    $ replicate 100_000
        { message: "warm"
        , temperature: 24.0
        }

overflow :: TestSuite
overflow =
  suite "overflow" do
    test "large data" do
      void <<< pure <<< f $ xs
  where
  f :: L.List Log -> CSVResult (L.List Log)
  f = parseCSV <<< printCSV
