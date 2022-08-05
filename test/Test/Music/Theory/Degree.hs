module Test.Music.Theory.Degree
  ( tests,
    genDegree,
  )
where

import Control.Exception
import Music.Theory
import Test.Hspec
import Test.Music.Theory.Accidental hiding (tests)
import Test.Music.Theory.Semitones hiding (tests)
import Test.QuickCheck
import Test.QuickCheck.Gen

instance Arbitrary Degree where
  arbitrary = elements [I .. XIII]
  shrink (Degree n) = Degree <$> [0 .. (n - 1)]

instance CoArbitrary Degree

instance Function Degree

genDegree :: Gen Degree
genDegree = elements [I .. XIII]

tests :: Spec
tests = describe "Degree" $ do
  specSemitones @Degree
  describe "Functions" $ do
    it "isPerfect" $
      and
        [ all isPerfect [I, IV, V, VIII],
          all (not . isPerfect) [II, VII, VI]
        ]
    it "toRoman" $ do
      evaluate (toRoman 0) `shouldThrow` anyException
      evaluate (toRoman (-1)) `shouldThrow` anyException
      toRoman 1 `shouldBe` "I"
      toRoman 4 `shouldBe` "IV"
      toRoman 9 `shouldBe` "IX"
      toRoman 11 `shouldBe` "XI"
      toRoman 14 `shouldBe` "XIV"
      toRoman 19 `shouldBe` "XIX"
      toRoman 1000 `shouldBe` "M"
      toRoman 1001 `shouldBe` "MI"
      toRoman 2000 `shouldBe` "MM"
