module Record.CSV.Printer.HeaderConstraint
  ( class HeaderConstraint
  , class SameNames
  ) where

import Prim.RowList as RL
import Record.CSV.Printer.SList (SCons, SNil, SList)
import Type.RowList (class ListToRow)

class HeaderConstraint (sl :: SList) (r :: Row Type) | sl -> r

instance headerConstraint ::
  ( RL.RowToList r rl
  , SameNames sl rl'
  , ListToRow rl' r
  ) =>
  HeaderConstraint sl r

class SameNames (sl :: SList) (rl :: RL.RowList Type) | sl -> rl

instance sameNamesNil :: SameNames SNil RL.Nil

instance sameNamesCons :: SameNames sl rl => SameNames (SCons s sl) (RL.Cons s t rl)
