{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Enpassing.Music.Key where

import           Control.Applicative
import           Control.Comonad
import           Euterpea.Music
import           Test.QuickCheck
import           Test.QuickCheck.Gen

type Key = (PitchClass, Mode)
data Keyed a = Keyed Key a deriving (Eq, Functor)

--Keyed Instances
instance Comonad Keyed where
  extract (Keyed k x) = x
  extend f keyed@(Keyed k _) =  Keyed k $ f keyed

instance Foldable Keyed where
  foldMap f (Keyed k x) = f x

instance Traversable Keyed where
  sequenceA (Keyed k trav) = fmap (Keyed k) trav

instance (Show s) => Show (Keyed s) where
  show (Keyed (pc, mode) s) = "("++ show s ++": "++ show pc ++ show mode ++")"

--Instances of Arbitrary
--Since Key is a product of arbitrary types, it automatically has an arbitrary instance
--The datatype `Keyed`
instance (Arbitrary a) => Arbitrary (Keyed a) where
  arbitrary = liftA2 Keyed arbitrary arbitrary

  shrink (Keyed k x) = liftA2 Keyed (shrink k) (shrink x)

instance Arbitrary Mode where
  arbitrary = elements [Major, Minor, Ionian, Dorian,
    Phrygian, Lydian, Mixolydian, Aeolian, Locrian,
    CustomMode "Aug", CustomMode "Dim"]

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
