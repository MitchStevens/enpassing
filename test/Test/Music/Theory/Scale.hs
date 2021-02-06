module Test.Music.Theory.Scale where

import Test.QuickCheck

import Music.Theory

instance Arbitrary Mode where
  arbitrary = newMode <$> traverse genInterval [I .. VII]
    where
      genInterval :: Degree -> Gen Interval
      genInterval deg = frequency
        [ (100, pure (newInterval Major deg))
        , (100, pure (newInterval Minor deg))
        , (10,  pure (newInterval Augmented deg))
        , (10,  pure (newInterval Diminished deg))
        ]

instance Arbitrary Scale where
  arbitrary = newScale <$> arbitrary <*> arbitrary

 wh


{- test scale like (Mode, Scale)
- test interval lens
- test semitones + transpose of scale
 - tes HasRoot of scale
- test that modes are equiv
-}
