module Record.CSV.Printer.SList
  ( kind SList
  , SNil
  , SCons
  , type (:)
  , LastSCons
  , type (:|)
  , SLProxy(..)
  , class ReflectSList
  , reflectSList
  ) where

import Prelude
import Data.List as L
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)

foreign import kind SList

foreign import data SNil :: SList

foreign import data SCons :: Symbol -> SList -> SList

infixr 6 type SCons as :

type LastSCons s l
  = SCons s (SCons l SNil)

infixr 6 type LastSCons as :|

data SLProxy (sl :: SList)
  = SLProxy

class ReflectSList (sl :: SList) where
  reflectSList :: SLProxy sl -> L.List String

instance reflectSListNil :: ReflectSList SNil where
  reflectSList _ = L.Nil

instance reflectSListCons ::
  ( IsSymbol s
  , ReflectSList sl
  ) =>
  ReflectSList (SCons s sl) where
  reflectSList _ = L.Cons name $ reflectSList (SLProxy :: SLProxy sl)
    where
    name = reflectSymbol (SProxy :: SProxy s)
