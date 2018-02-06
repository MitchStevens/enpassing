{-# LANGUAGE FlexibleInstances #-}

module Enpassing.Changes.Style (
  Style (..),
  stylise,
  styles
) where

import           Data.Bifunctor
import           Data.Monoid
import           Enpassing.Changes.Passing
import           Enpassing.Changes.Substitution
import Enpassing.Changes.ChordModification
import           Enpassing.Music
import           Euterpea.Music
import Data.Map.Strict (Map, fromList)
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

styles :: Map String Style
styles = fromList
  [ ("None",        none)
  , ("Basic Jazz",  basic_jazz)
  , ("Simplify",    simplify)
  , ("Moody",       moody)]

none = Style "(You've got) No Style" [] []

basic_jazz = Style "Basic Jazz"
  [ (100, no_sub)
  , (100, sub_7th)
  , (50,  sub_9th)
  , (10,  sub_11th)
  , (10,  sub_13th)
  , (30,  sub_tritone)
  , (30,  sub_relative)
  ] []

simplify = Style "Simplify"
  [(100, sub_simplify)]
  []

moody = Style "Moody" [] []