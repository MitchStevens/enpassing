module Test.Music.Theory.Interval where

import Music.Theory
import Test.Music.Theory.Accidental
import Test.Music.Theory.Degree
import Test.Music.Theory.Semitones
import Test.QuickCheck

instance Arbitrary Interval where
  arbitrary = Interval <$> arbitrary <*> arbitrary
  shrink (Interval accidental degree) =
    Interval <$> shrink accidental <*> shrink degree

instance CoArbitrary Interval where
  coarbitrary = genericCoarbitrary

instance Function Interval

genInterval :: Gen Interval
genInterval = Interval <$> genAccidental <*> genDegree
