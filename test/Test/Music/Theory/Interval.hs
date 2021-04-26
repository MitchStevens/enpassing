module Test.Music.Theory.Interval where

import Test.QuickCheck
import Data.Validity

import Music.Theory.Interval
import Test.Music.Theory.Degree

instance Validity Interval where
  validate _ = valid

instance Arbitrary Interval where
  arbitrary = Interval
    <$> (oneOf [Major, Minor, Diminished, Augmented])
    <*> (oneOf [I, II, III, IV, V, VI, VII])
