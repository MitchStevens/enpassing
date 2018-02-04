{-# LANGUAGE FlexibleInstances #-}

module Enpassing.Changes.Style where

import           Data.Bifunctor
import           Data.Monoid
import           Enpassing.Changes.Passing
import           Enpassing.Changes.Substitution
import Enpassing.Changes.ChordModification
import           Enpassing.Music
import           Euterpea.Music
import           Test.QuickCheck.Gen
import Data.Semigroup

data Style = Style
  { style_name :: String
  , substitutions :: [(Int, Substitution)]
  , additions     :: [(Int, Passing)]
  }

instance (Modification m) => Modification [(Int, m)] where
  name _ = "Not used"
  situational tups chord =
    any (\m -> situational m chord) . fmap snd $ tups

  generator tups chord = frequency
                . fmap (\(n, m) -> (n, generator m chord))
                . filter ((\m -> situational m chord) . snd) $ tups

instance Semigroup Style where
  Style n1 s1 p1 <> Style n2 s2 p2 = Style (n1++" + "++n2) (s1++s2) (p1++p2)

stylise :: Style -> Sheet -> IO Sheet
stylise (Style _ subs adds) sheet = pure sheet >>= modify subs >>= modify adds


basic_style = Style "Basic Style"
  [ --(50, no_sub)
  --, (20, tritone_sub)
  (50, sharp_i_replaces_VI) ]
  [ (40, no_addition)
  , (20, tritone_addition)]
