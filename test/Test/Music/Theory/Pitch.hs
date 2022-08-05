module Test.Music.Theory.Pitch where

import GHC.Generics
import Music.Theory
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.Music.Theory.Accidental hiding (tests)
import Test.Music.Theory.Transpose hiding (tests)
import Test.QuickCheck

instance Arbitrary PitchClass where
  arbitrary = elements allPitchClasses

instance Arbitrary Pitch where
  arbitrary = (%) <$> (arbitrary :: Gen PitchClass) <*> arbitrary

instance CoArbitrary NoteName

instance CoArbitrary PitchClass

instance CoArbitrary Pitch

--instance CoArbitrary NoOctave

instance Function NoteName

instance Function PitchClass

instance Function Pitch

--instance Function NoOctave

tests :: Spec
tests = do
  specPitchClass
  specPitch

specPitchClass :: Spec
specPitchClass = describe "PitchClass" $ do
  specSemitones @PitchClass
  specWeakTransposition @PitchClass
  specHasAccidental @PitchClass
  specEnharmonic @PitchClass
  describe "Functions" $ do
    it "shift" $ do
      shift "P1" c `shouldBe` c
      shift "M2" c `shouldBe` d
      shift "m3" b `shouldBe` d
      shift "m3" d `shouldBe` f
      shift "M3" g `shouldBe` b
      shift "P4" a `shouldBe` d
      shift "P5" a `shouldBe` e
      shift "m7" c `shouldBe` (flat b)
      shift "P8" a `shouldBe` a
    it "enharmonic" $ do
      sharpKey c `shouldBe` c
      flatKey c `shouldBe` c
      sharpKey (sharp a) `shouldBe` sharp a
      sharpKey (flat b) `shouldBe` sharp a

specPitch :: Spec
specPitch = describe "Pitch" $ do
  specSemitones @Pitch
  specStrongTransposition @Pitch
  --specEnharmonic @Pitch
  describe "Functions" $ do
    it "steps" $ do
      steps (c % 0) `shouldBe` 0
      steps (flat c % 0) `shouldBe` (-1)
      steps (sharp c % 0) `shouldBe` 1
    it "shift" $ do
      shift "P1" (c % 0) `shouldBe` (c % 0)
      shift "P1" (flat c % 0) `shouldBe` (flat c % 0)
      shift "A5" (flat g % 0) `shouldBe` (d % 1)
      shift "P1" (b % 0) `shouldBe` (b % 0)
      shift "m2" (b % 0) `shouldBe` (c % 1)
      shift "m9" (b % 0) `shouldBe` (c % 2)
      shift "d4" (g % 1) `shouldBe` (flat c % 2)
      shift (Interval DoubleFlat (Degree 2)) (e % 0) `shouldBe` (flat g % 0)
      shift (Interval DoubleFlat (Degree 1)) (flat e % 5) `shouldBe` (flat $ flat f % 5)
    it "transposition property" $ do
      let note = flat a % 0
      --steps note `shouldBe` 8
      let interval = Interval DoubleFlat IV
      --steps interval `shouldBe` 3
      shift interval note `shouldBe` (flat $ doubleFlat (d % 1))
      steps (shift interval note) `shouldBe` steps interval + steps note
