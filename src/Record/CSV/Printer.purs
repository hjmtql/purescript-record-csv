module Record.CSV.Printer
  ( printCSV
  , printCSVWithOrder
  ) where

import Prelude
import Data.Array as A
import Data.Either (fromRight)
import Data.List as L
import Data.String (joinWith)
import Prim.RowList as RL
import Record.CSV.Header (class Header, headerItems)
import Record.CSV.OrderHelper (pickHeaderOrder, sortColumns)
import Record.CSV.Printer.HeaderConstraint (class HeaderConstraint)
import Record.CSV.Printer.PrintValues (class PrintValues, printValues)
import Record.CSV.Printer.SList (class ReflectSList, SLProxy(..), reflectSList)
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

printCSVWithOrder ::
  forall r rl sl.
  RL.RowToList r rl =>
  PrintValues rl r =>
  Header rl =>
  ReflectSList sl =>
  HeaderConstraint sl r =>
  SLProxy sl -> L.List { | r } -> String
printCSVWithOrder _ r = mkCol <<< map mkRow <<< L.Cons ohs $ orderedValues
  where
  -- NOTE: it can pick all the row header by type class constraint `HeaderConstraint`
  -- NOTE: so use fromRight L.Nil for convenience
  orderedValues =
    fromRight L.Nil do
      order <- pickHeaderOrder ohs hs
      sortColumns order values

  hs = headerItems (Proxy :: Proxy { | r })

  ohs = reflectSList (SLProxy :: SLProxy sl)

  values = map printValues r

mkRow :: L.List String -> String
mkRow = joinWith "," <<< A.fromFoldable

mkCol :: L.List String -> String
mkCol = joinWith "\n" <<< A.fromFoldable
