module Enpassing.Theory.Degree where

import           Enpassing.Theory.Types

import           Control.Comonad
import           Control.Lens
import           Data.Char          (toLower, toUpper)
import           Data.Functor       (($>))
import           Data.Maybe         (mapMaybe)

instance Semigroup Degree where
  Degree m <> Degree n = Degree (m+n -1)
instance Monoid Degree where
  mempty = Degree 1

allDegrees :: [Degree]
allDegrees = mapMaybe (^? mkDegree) [1..13]

[d1, d2, d3, d4, d5, d6, d7, d8, d9, d10, d11, d12, d13] =
  allDegrees

mkDegree :: Prism' Int Degree
mkDegree = prism'
  (\(Degree n) -> n)
  (\n -> if n > 0 then Just (Degree n) else Nothing)

_LetterCase :: Lens' (LetterCase a) a
_LetterCase = lens (\case { Upper x -> x; Lower x -> x })
 ($>)

