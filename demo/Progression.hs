-- |

module Progression where

import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Control.Comonad

-- Progression is a zipper, it is defined as a new datatype to extend it
-- later.
data Progression a = Progression
  { lefts :: [a]
  , selected :: s
  , rights :: [a]
  }

instance Functor Progression where
  fmap f (Progression l s r) =
    Progression (fmap f l) (f s) (fmap f r)

instance Foldable Progression where
  foldMap f (Progression l s r) =
    foldMap f l <> f s <> foldMap f r

instance Traversable Progression where
  traverse f (Progression l s r) =
    Progression
      <$> traverse f l
      <*> f s
      <*> traverse f r

instance Comonad Progression where
  extract = selected
  duplicate p@(Progression l s r) =
    Progression (allLefts p) p (allRights p)

allLefts :: Progression a -> [Progression a]
allLefts = unfold go
  where
    go @p(Progression lefts s r) = case lefts of
      l:ls -> Cons p (Progression ls l (s:r))
      []   -> Nil

allRights :: Progression a -> [Progression a]
allRights = unfold go
  where
    go p@(Progression l s rights) = case rights of
      r:rs -> Cons p (Progression (s:l) r rs)
      []   -> Nil
