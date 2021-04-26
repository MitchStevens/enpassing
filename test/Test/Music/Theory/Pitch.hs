module Test.Music.Theory.Pitch where

import Test.Hspec
import Test.Hspec.QuickCheck
import Data.Validity
import Test.QuickCheck

import Music.Theory
import Test.Music.Theory.Transpose

instance Validity Note where
  validate _ = valid

instance Arbitrary Note where
  arbitrary = elements [C, D, E, F, G, A, B]


instance Validity PitchClass where
  validate _ = valid

instance Arbitrary PitchClass where
  arbitrary = elements allPitchClasses


instance Validity Pitch where
  validate _ = valid

instance Arbitrary Pitch where
  arbitrary = Pitch <$> arbitrary <*> arbitrary

tests :: Spec
tests = do
  specPitchClass
  specPitch

specPitchClass :: Spec
specPitchClass = describe "PitchClass" $ do
  describe "`pitchClass`" $ do
    it "" $ sequence_ (zipWith shouldBe (pitchClass <$> [0..11]) allPitchClasses)
    prop "" $ (\p -> p === (pitchClass . steps) p)
    prop "" $ (\x y -> (mod12 (x - y) == 0) === (pitchClass x == pitchClass y))
  specWeakTransposition @PitchClass

specPitch :: Spec
specPitch = describe "Pitch" $ do
  it "" $
    property (\p -> p === (mkPitch . steps) p)
  specStrongTransposition @Pitch
