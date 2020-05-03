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
import Record.CSV.Header (class Header, headerItems)
import Record.CSV.Parser.ParseAsString (parseAsString)
import Record.CSV.Parser.ParseValues (class ParseValues, parseValues)
import Type.Proxy (Proxy(..))

parseCSV ::
  forall rl r.
  RL.RowToList r rl =>
  ParseValues rl r =>
  Header rl =>
  String -> Either String (L.List { | r })
parseCSV s = do
  -- NOTE: First, parse csv as String
  csv <- lmap show $ parseAsString s
  -- NOTE: Second, check header
  let
    header = headerItems (Proxy :: Proxy { | r })
  case L.head csv of
    Just h
      | h == header -> Right unit
    Just _ -> Left "Mismatch header."
    Nothing -> Left "No header."
  -- NOTE: Finally, parse values
  let
    vals = fromMaybe L.Nil $ L.tail csv
  traverse parseValues vals
