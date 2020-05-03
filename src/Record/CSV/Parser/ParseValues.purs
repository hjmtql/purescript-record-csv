module Record.CSV.Parser.ParseValues
  ( class ParseValues
  , parseProxy
  , parseValues
  ) where

import Prelude
import Data.Either (Either(..))
import Data.List as L
import Data.Symbol (class IsSymbol, SProxy(..))
import Prim.Row as R
import Prim.RowList as RL
import Record as Record
import Record.CSV.Parser.FromCSV (class FromCSV, fromCSV)
import Type.Data.RowList (RLProxy(..))

class ParseValues (rl :: RL.RowList) (r :: # Type) | rl -> r where
  parseProxy :: RLProxy rl -> L.List String -> Either String { | r }

instance parseValuesNil :: ParseValues RL.Nil () where
  parseProxy _ L.Nil = Right {}
  parseProxy _ (L.Cons x xs) = Left "Values are longer than record keys."

instance parseValuesCons ::
  ( ParseValues rl tail
  , R.Cons name a tail row
  , R.Lacks name tail
  , IsSymbol name
  , FromCSV a
  ) =>
  ParseValues (RL.Cons name a rl) row where
  parseProxy _ L.Nil = Left "Values are shorter than record keys."
  parseProxy _ (L.Cons x xs) = case fromCSV x of
    Right v -> Record.insert nameP v <$> parseProxy rlP xs
    Left e -> Left e
    where
    rlP = RLProxy :: RLProxy rl

    nameP = SProxy :: SProxy name

parseValues ::
  forall r rl.
  RL.RowToList r rl =>
  ParseValues rl r =>
  L.List String -> Either String { | r }
parseValues = parseProxy (RLProxy :: RLProxy rl)
