module Test.Music.Theory.Accidental (tests) where

import Test.QuickCheck
import Test.Hspec

import Music.Theory.Accidental

instance Arbitrary Accidental where
  arbitrary = elements [DoubleFlat, Flat, Natural, Sharp, DoubleSharp]

tests :: SpecWith ()
tests = describe "Accidental" $ do
  pure ()
