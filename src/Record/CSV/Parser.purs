module Record.CSV.Parser
  ( parseCSV
  ) where

import Prelude
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.List as L
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Traversable (traverse)
import Prim.RowList as RL
import Record.CSV.Error (CSVError(..), fromParseError)
import Record.CSV.Header (class Header, headerItems)
import Record.CSV.OrderHelper (pickHeaderOrder, sortColumns)
import Record.CSV.Parser.ParseAsString (parseAsString)
import Record.CSV.Parser.ParseValues (class ParseValues, parseValues)
import Record.CSV.Parser.ShapeChecker (sameLength)
import Record.CSV.Type (CSVResult)
import Type.Proxy (Proxy(..))

parseCSV ::
  forall rl r.
  RL.RowToList r rl =>
  ParseValues rl r =>
  Header rl =>
  String -> CSVResult (L.List { | r })
parseCSV s = do
  -- NOTE: First, parse csv as String
  csv <- lmap fromParseError $ parseAsString s
  let
    filteredCsv = L.filter notEmptyLine csv
  checkedCsv <- sameLength filteredCsv
  -- NOTE: Second, pick sort order from row header
  order <- case L.head checkedCsv of
    Just hs -> pickHeaderOrder rhs hs
    Nothing -> Left $ NoLine "There is no csv line."
  -- NOTE: Finally, parse values
  let
    values = fromMaybe L.Nil $ L.tail checkedCsv
  sortedValues <- sortColumns order values
  traverse parseValues sortedValues
  where
  rhs = headerItems (Proxy :: Proxy { | r })

  notEmptyLine = notEq (L.singleton "")
