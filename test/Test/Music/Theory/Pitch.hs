module Test.Music.Theory.Pitch where

import Test.Hspec
import Test.Hspec.QuickCheck
import Data.Validity
import Test.QuickCheck

import Music.Theory
import Test.Music.Theory.Transpose


instance Arbitrary PitchClass where
  arbitrary = elements allPitchClasses

instance Arbitrary Pitch where
  arbitrary = (%) <$> arbitrary <*> arbitrary

tests :: Spec
tests = do
  specPitchClass
  specPitch

specPitchClass :: Spec
specPitchClass = describe "PitchClass" $ do
  --it "" $ sequence_ (zipWith shouldBe (pitchClass <$> [0..11]) allPitchClasses)
  --prop "" $ (\p -> p === (pitchClass . steps) p)
  --prop "" $ (\x y -> (mod12 (x - y) == 0) === (pitchClass x == pitchClass y))
  specSemitones @Pitch
  specWeakTransposition @PitchClass

specPitch :: Spec
specPitch = describe "Pitch" $ do
  --it "" $
  --  property (\p -> p === (mkPitch . steps) p)
  specSemitones @Pitch
  specStrongTransposition @Pitch
