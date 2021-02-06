module Test.Music.Theory.Interval where

import Music.Theory.Interval
import Test.Music.Theory.Degree

import Test.QuickCheck

instance Arbitrary Interval where
  arbitrary = Interval <$> arbitrary <*> arbitrary
