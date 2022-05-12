module Record.CSV.Parser.ParseValues
  ( class ParseValues
  , parseProxy
  , parseValues
  ) where

import Prelude
import Data.Either (Either(..))
import Data.List as L
import Data.Symbol (class IsSymbol)
import Prim.Row as R
import Prim.RowList as RL
import Record as Record
import Record.CSV.Error (CSVError(..))
import Record.CSV.Parser.FromCSV (class FromCSV, fromCSV)
import Record.CSV.Type (CSVResult, CSVLine)
import Type.Proxy (Proxy(..))

class ParseValues (rl :: RL.RowList Type) (r :: Row Type) | rl -> r where
  parseProxy :: Proxy rl -> CSVLine -> CSVResult { | r }

instance parseValuesNil :: ParseValues RL.Nil () where
  parseProxy _ L.Nil = Right {}
  parseProxy _ (L.Cons _ _) = Left Unreachable

instance parseValuesCons ::
  ( ParseValues rl tail
  , R.Cons name a tail row
  , R.Lacks name tail
  , IsSymbol name
  , FromCSV a
  ) =>
  ParseValues (RL.Cons name a rl) row where
  parseProxy _ L.Nil = Left Unreachable
  parseProxy _ (L.Cons x xs) = case fromCSV x of
    Right v -> Record.insert nameP v <$> parseProxy rlP xs
    Left e -> Left e
    where
    rlP = Proxy :: Proxy rl

    nameP = Proxy :: Proxy name

parseValues ::
  forall r rl.
  RL.RowToList r rl =>
  ParseValues rl r =>
  CSVLine -> CSVResult { | r }
parseValues = parseProxy (Proxy :: Proxy rl)
