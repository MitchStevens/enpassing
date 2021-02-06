module Test.Music.Theory.Pitch where

import           Music.Theory
import Test.Music.Theory.Transpose

import           Test.Hspec
import           Test.QuickCheck

instance Arbitrary Note where
  arbitrary = elements [C, D, E, F, G, A, B]

instance Arbitrary PitchClass where
  arbitrary = elements allPitchClasses

instance Arbitrary Pitch where
  arbitrary = Pitch <$> arbitrary <*> arbitrary

tests :: Spec
tests = do
  specPitchClass
  specPitch

specPitchClass :: Spec
specPitchClass = describe "PitchClass" $ do
  describe "`pitchClass`" $ do
    it "" $ sequence_
      (zipWith shouldBe (pitchClass <$> [0..11]) allPitchClasses)
    it "" $
      property (\p -> p === (pitchClass . steps) p)
    it "" $
      property (\x y -> (mod12 (x - y) == 0) === (pitchClass x == pitchClass y))
  specWeakTransposition @PitchClass

specPitch :: Spec
specPitch = describe "Pitch" $ do
  it "" $
    property (\p -> p === (mkPitch . steps) p)
  specStrongTransposition @Pitch
