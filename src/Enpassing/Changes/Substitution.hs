{-# LANGUAGE FlexibleInstances, ExplicitForAll, UndecidableInstances #-}

module Enpassing.Changes.Substitution where

import Test.QuickCheck
import Enpassing.Music
import Enpassing.Changes.ChordPredicate as ChordPredicate
import Euterpea.Music
import Control.Comonad
import Data.Functor.Compose
import Data.Maybe
import Data.Semigroup hiding (Min)

class Substitutable s where
  substitute :: s -> Gen s

instance Substitutable (Keyed Chord) where
  substitute k@(_, chord) = if p chord then g k else pure k
    where Substitution _ _ p g = no_sub -- <> tritone_sub

instance (Substitutable (Keyed a), Traversable t) => Substitutable (Keyed (t a)) where
  substitute = fmap sequence . sequence . fmap substitute . sequence :: t (Gen (Keyed a))

a (b (c d)) -> c (a (b d))

instance Substitutable Sheet where
  substitute (Sheet name key bars) = Sheet name key <$> concat bars

--Keyed [Bar] :: Keyed [[Chord]]

{-
  A substitution is a datastructure describing a possible chord substitution.
  If a chord satisfies a condition, return Just a generator for that chord, else return Nothing
-}
data Substitution = Substitution {
  sub_name :: String,
  sub_prob :: Int,
  predicate :: ChordPredicate,
  generator :: Keyed Chord -> Gen (Keyed Chord)}

instance Semigroup Substitution where
  Substitution n1 d1 p1 g1 <> Substitution n2 d2 p2 g2 = Substitution n3 d3 p3 g3
    where
      n3 = n1++", "++n2
      d3 = d1+d2
      p3 c = p1 c && p2 c
      g3 p = frequency [(d1, g1 p), (d2, g2 p)]

-- Substitutions
no_sub = Substitution "No Substitution" 40 ChordPredicate.true pure

--Extra instances for Primitive
instance Foldable Primitive where
  foldMap f (Note _ x) = f x
  foldMap _ _ = mempty

instance Traversable Primitive where
  traverse f (Note d x) = Note d <$> f x
  traverse f (Rest d)   = pure $ Rest d