{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Enpassing.Music.Key where

import           Control.Comonad
import           Euterpea.Music
import           Test.QuickCheck
import           Test.QuickCheck.Gen

type Key = (PitchClass, Mode)
data Keyed a = Keyed Key a deriving (Functor)

--Keyed Instances
instance Comonad Keyed where
  extract (Keyed k x) = x
  extend f keyed@(Keyed k _) =  Keyed k $ f keyed

instance Foldable Keyed where
  foldMap f (Keyed k x) = f x

instance Traversable Keyed where
  sequence (Keyed k trav) = fmap (Keyed k) trav

--Instances of Arbitrary
instance Arbitrary Key where
  arbitrary = (,) <$> arbitrary <*> arbitrary

  shrink (C, Major) = []
  shrink x          = [(C, Major)]


instance Arbitrary Mode where
  arbitrary = elements [Major, Minor, Ionian, Dorian, Phrygian,
    Lydian, Mixolydian, Aeolian, Locrian, CustomMode "Augmented", CustomMode "Octatonic"]

  shrink Major      = []
  shrink Minor      = []
  shrink Mixolydian = []
  shrink x          = [Major, Minor, Mixolydian]

instance Arbitrary PitchClass where
  arbitrary = arbitraryBoundedEnum

  shrink C = []
  shrink F = []
  shrink G = []
  shrink x = [C, F, G]
