module Test.Music.Theory.Chord (tests) where

import Debug.Trace
import Music.Theory
import Test.Hspec
import Test.Music.Theory.Classes hiding (tests)
import Test.Music.Theory.Pitch hiding (tests)
import Test.Music.Theory.Transpose hiding (tests)
import Test.QuickCheck

instance Arbitrary a => Arbitrary (Chord a) where
  arbitrary = do
    root <- arbitrary
    pure $ chord root Major []

tests :: Spec
tests = focus $ do
  describe "Chord" $ do
    specSemitones @(Chord ())
    specChordPitchClass
    specChordPitch

specChordPitchClass :: Spec
specChordPitchClass =
  describe "Chord PitchClass" $ do
    describe "Functions" $ do
      it "equality" $ do
        chord c Major [] `shouldBe` chord c Major []
      it "triad" $ do
        triad Major `shouldBe` ["P1", "M3", "P5"]
        triad Minor `shouldBe` ["P1", "m3", "P5"]
      it "shift" $ do
        let cmaj = chord c Major []
        let interval = Interval Natural II
        shift interval cmaj `shouldBe` chord d Major []
      it "transpose" $ do
        let twoFiveOneC = [chord d Minor [], chord g Major [], chord c Major []]
        let twoFiveOneBb = [chord c Minor [], chord f Major [], chord (flat b) Major []]
        map (transpose (-2)) twoFiveOneC `shouldBe` twoFiveOneBb
        map (transpose 12) twoFiveOneC `shouldBe` twoFiveOneC
      it "arpeggiate" $ do
        arpeggiate (chord c Major []) `shouldBe` [c, e, g]
        arpeggiate (chord c Major ["M7"]) `shouldBe` [c, e, g, b]
        arpeggiate (chord c Major ["M7", "M9"]) `shouldBe` [c, e, g, b, d]
        arpeggiate (chord g Major ["M7"]) `shouldBe` [g, b, d, sharp f]
        arpeggiate (chord (sharp f) Minor ["m7"]) `shouldBe` [sharp f, a, sharp c, e]
        arpeggiate (chord (flat b) Major ["M7", "M9", "M11", "M13"]) `shouldBe` [flat b, d, f, a, c, flat e, g]
    specSemitones @(Chord PitchClass)
    specWeakTransposition @(Chord PitchClass)
    specHasRoot @(Chord PitchClass)
    specHasIntervals @(Chord PitchClass)

specChordPitch :: Spec
specChordPitch = describe "Chord Pitch" $ do
  describe "Functions" $ do
    it "arpeggiate" $ do
      arpeggiate (chord (c % 5) Major []) `shouldBe` [c % 5, e % 5, g % 5]
      arpeggiate (chord (c % 5) Major ["M7"]) `shouldBe` [c % 5, e % 5, g % 5, b % 5]
      arpeggiate (chord (c % 5) Major ["M7", "M9"]) `shouldBe` [c % 5, e % 5, g % 5, b % 5, d % 6]
      arpeggiate (chord (g % 5) Major ["M7"]) `shouldBe` [g % 5, b % 5, d % 6, sharp f % 6]
      arpeggiate (chord (flat b % 5) Major ["M7", "M9", "M11", "M13"]) `shouldBe` [flat b % 5, d % 6, f % 6, a % 6, c % 7, flat e % 7, g % 7]
  specSemitones @(Chord Pitch)
  specStrongTransposition @(Chord Pitch)

--[c4, cs4, d4, ds4, e4, f4, fs4, g4, gs4, a4, as4, b4] = map (\x -> trans x (C % 4)) [0 .. 11]
--[c5, cs5, d5, ds5, e5, f5, fs5, g5, gs5, a5, as5, b5] = map (\x -> trans x (C % 5)) [0 .. 11]
