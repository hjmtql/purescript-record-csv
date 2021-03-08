module Record.CSV.Header
  ( class Header
  , headerProxy
  , headerItems
  ) where

import Prelude
import Data.List as L
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Prim.RowList as RL
import Record.CSV.Type (CSVLine)
import Type.Data.RowList (RLProxy(..))
import Type.Proxy (Proxy)

class Header (rl :: RL.RowList Type) where
  headerProxy :: RLProxy rl -> CSVLine

instance headerNil :: Header RL.Nil where
  headerProxy _ = L.Nil

instance headerCons ::
  ( Header rl
  , IsSymbol name
  ) =>
  Header (RL.Cons name t rl) where
  headerProxy _ = L.Cons key $ headerProxy rlP
    where
    rlP = RLProxy :: RLProxy rl

    nameP = SProxy :: SProxy name

    key = reflectSymbol nameP

headerItems ::
  forall r rl.
  RL.RowToList r rl =>
  Header rl =>
  Proxy { | r } -> CSVLine
headerItems _ = headerProxy (RLProxy :: RLProxy rl)
