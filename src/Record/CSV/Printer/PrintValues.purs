module Record.CSV.Printer.PrintValues
  ( class PrintValues
  , printProxy
  , printValues
  ) where

import Prelude
import Data.List as L
import Data.Symbol (class IsSymbol)
import Prim.Row as R
import Prim.RowList as RL
import Record as Record
import Record.CSV.Printer.ToCSV (class ToCSV, toCSV)
import Type.Proxy (Proxy(..))

class PrintValues (rl :: RL.RowList Type) (r :: Row Type) | rl -> r where
  printProxy :: Proxy rl -> { | r } -> L.List String

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
  printProxy _ r = L.Cons val $ printProxy (Proxy :: Proxy rl) nr
    where
    nameP = Proxy :: Proxy name

    val = toCSV $ Record.get nameP r

    nr = Record.delete nameP r

printValues ::
  forall r rl.
  RL.RowToList r rl =>
  PrintValues rl r =>
  { | r } -> L.List String
printValues = printProxy (Proxy :: Proxy rl)
