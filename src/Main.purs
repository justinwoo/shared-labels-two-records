module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log, logShow)
import Data.Array as Array
import Data.List (List, (:))
import Data.Monoid (mempty)
import Type.Data.Boolean (class If, class Or)
import Type.Data.Ordering (class Equals)
import Type.Prelude (class CompareSymbol, class IsSymbol, class RowToList, EQ, LT, RLProxy(..), SProxy(..), reflectSymbol)
import Type.Row (Cons, Nil, kind RowList)

class Keys (xs :: RowList) where
  keysImpl :: RLProxy xs -> List String

instance nilKeys :: Keys Nil where
  keysImpl _ = mempty

instance consKeys ::
  ( IsSymbol name
  , Keys tail
  ) => Keys (Cons name ty tail) where
  keysImpl _ = first : rest
    where
      first = reflectSymbol (SProxy :: SProxy name)
      rest = keysImpl (RLProxy :: RLProxy tail)

class RowListIntersection
  (xs :: RowList)
  (ys :: RowList)
  (res :: RowList)
  | xs ys -> res

instance rliNilXS :: RowListIntersection Nil (Cons name ty tail) Nil
instance rliNilYS :: RowListIntersection (Cons name ty tail) Nil Nil
instance rliNilNil :: RowListIntersection Nil Nil Nil
instance rliConsCons ::
  ( CompareSymbol xname yname ord
  , Equals ord EQ isEq
  , Equals ord LT isLt
  , Or isEq isLt isEqOrLt
  , If isEq xty trashty yty
  , If isEq xty trashty2 zty
  , If isEq (SProxy xname) trashname (SProxy zname)
  , If isEq
      (RLProxy (Cons zname zty res'))
      (RLProxy res')
      (RLProxy res)
  , If isEqOrLt
      (RLProxy xs)
      (RLProxy (Cons xname xty xs))
      (RLProxy xs')
  , If isLt
      (RLProxy (Cons xname yty ys))
      (RLProxy ys)
      (RLProxy ys')
  , RowListIntersection xs' ys' res'
  ) => RowListIntersection (Cons xname xty xs) (Cons yname yty ys) res

getSharedLabels
  :: forall r1 rl1 r2 rl2 rl
   . RowToList r1 rl1
  => RowToList r2 rl2
  => RowListIntersection rl1 rl2 rl
  => Keys rl
  => Record r1
  -> Record r2
  -> List String
getSharedLabels _ _ = keysImpl (RLProxy :: RLProxy rl)


main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  logShow <<< Array.fromFoldable $
    getSharedLabels
      { a: 123, b: "abc" }
      { a: 123, b: "abc", c: true }
  log "Hello sailor!"
