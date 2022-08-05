module Test.Music.Theory.Scale where

import Music.Theory
import Test.QuickCheck

instance Arbitrary Mode where
  arbitrary = newMode <$> traverse genInterval [I .. VII]
    where
      genInterval :: Degree -> Gen Interval
      genInterval deg =
        frequency
          [ (100, pure (Interval Major deg)),
            (100, pure (Interval Minor deg)),
            (10, pure (Interval Augmented deg)),
            (10, pure (Interval Diminished deg))
          ]

instance Arbitrary Scale where
  arbitrary = newScale <$> arbitrary <*> arbitrary

{- test scale like (Mode, Scale)
- test interval lens
- test semitones + transpose of scale
 - tes HasRoot of scale
- test that modes are equiv
-}
