module Record.CSV.Printer
  ( printCSV
  , printCSVWithOrder
  ) where

import Prelude
import Data.Array as A
import Data.List as L
import Data.String (joinWith)
import Prim.RowList as RL
import Record.CSV.Header (class Header, headerItems)
import Record.CSV.OrderHelper (pickHeaderOrder, sortColumns)
import Record.CSV.Printer.HeaderConstraint (class HeaderConstraint)
import Record.CSV.Printer.PrintValues (class PrintValues, printValues)
import Record.CSV.Printer.SList (class ReflectSList, SLProxy(..), reflectSList)
import Record.CSV.Type (CSVResult)
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
  L.List { | r } -> SLProxy sl -> CSVResult String
printCSVWithOrder r _ = do
  -- NOTE: it can pick all the row header by type constraint `HeaderConstraint`
  order <- pickHeaderOrder ohs hs
  orderdValues <- sortColumns order values
  pure <<< mkCol <<< map mkRow $ L.Cons ohs orderdValues
  where
  hs = headerItems (Proxy :: Proxy { | r })

  ohs = reflectSList (SLProxy :: SLProxy sl)

  values = map printValues r

mkRow :: L.List String -> String
mkRow = joinWith "," <<< A.fromFoldable

mkCol :: L.List String -> String
mkCol = joinWith "\n" <<< A.fromFoldable
