module Test.Music.Theory.Quality where

import Music.Theory.Quality

import Test.QuickCheck


instance Arbitrary Quality where
  arbitrary = elements [Major, Minor, Diminished, Augmented]



