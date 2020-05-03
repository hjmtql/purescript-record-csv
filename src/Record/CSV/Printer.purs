module Record.CSV.Printer
  ( printCSV
  ) where

import Prelude
import Data.Array as A
import Data.List as L
import Data.String (joinWith)
import Prim.RowList as RL
import Record.CSV.Header (class Header, headerItems)
import Record.CSV.Printer.PrintValues (class PrintValues, printValues)
import Type.Proxy (Proxy(..))

printCSV ::
  forall r rl.
  RL.RowToList r rl =>
  PrintValues rl r =>
  Header rl =>
  L.List { | r } -> String
printCSV r = mkCol <<< map mkRow $ L.Cons header values
  where
  header = headerItems (Proxy :: Proxy { | r })

  values = map printValues r

  mkRow :: L.List String -> String
  mkRow = joinWith "," <<< A.fromFoldable

  mkCol :: L.List String -> String
  mkCol = joinWith "\n" <<< A.fromFoldable
