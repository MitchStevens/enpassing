module Test.Music.Theory.MusicalBase where

import Data.Validity
import Test.Hspec
import Data.Foldable

import Music.Theory

data MusicalBase a = MusicalBase { _base :: !a, _offsets :: ![Interval] }

instance Validity a => Validity (MusicalBase a) where
  validate mb@(MusicalBase base offsets) = fold
    [ valid base
    , check "" (isDegenerate mb)
    , check "" (length offsets <= 7)
    ]

instance Arbitrary a => Arbitrary (MusicalBase a) where
  arbitrary = 

