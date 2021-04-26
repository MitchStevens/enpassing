module Test.Music.Theory.Chord (tests) where

import Data.Validity
import Test.Hspec
import Test.QuickCheck

import Music.Theory
import Test.Music.Theory.Transpose

-- TODO: create instances for validity, arbitraray
instance Validity ChordType where
  validate _ = valid
instance Validity Chord where
  validate _ = valid
instance Validity ChordPrecise where
  validate _ = valid

instance Arbitrary ChordType where
  arbitrary = pure (chord () Major [])
instance Arbitrary Chord where
  arbitrary = pure (chord c Major [])
instance Arbitrary ChordPrecise where
  arbitrary = pure (chord (c%5) Major [])
  
tests :: Spec
tests = do
  specWeakTransposition @ChordType
  specWeakTransposition @Chord
  specWeakTransposition @ChordPrecise
