module Record.CSV.Printer.PrintValues
  ( class PrintValues
  , printProxy
  , printValues
  ) where

import Prelude
import Data.List as L
import Data.Symbol (class IsSymbol, SProxy(..))
import Prim.Row as R
import Prim.RowList as RL
import Record as Record
import Record.CSV.Printer.ToCSV (class ToCSV, toCSV)
import Type.Data.RowList (RLProxy(..))

class PrintValues (rl :: RL.RowList) (r :: # Type) | rl -> r where
  printProxy :: RLProxy rl -> { | r } -> L.List String

instance printValuesNil :: PrintValues RL.Nil () where
  printProxy _ _ = L.Nil

instance printValuesCons ::
  ( PrintValues rl tail
  , R.Cons name a tail row
  , R.Lacks name tail
  , IsSymbol name
  , ToCSV a
  ) =>
  PrintValues (RL.Cons name a rl) row where
  printProxy _ r = L.Cons val $ printProxy (RLProxy :: RLProxy rl) nr
    where
    nameP = SProxy :: SProxy name

    val = toCSV $ Record.get nameP r

    nr = Record.delete nameP r

printValues ::
  forall r rl.
  RL.RowToList r rl =>
  PrintValues rl r =>
  { | r } -> L.List String
printValues = printProxy (RLProxy :: RLProxy rl)
