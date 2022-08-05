module Test.Music.Theory.Accidental where

import Control.Lens hiding (elements)
import Control.Lens.Properties (isLens)
import Music.Theory
import Test.Function
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.Music.Theory.Semitones hiding (tests)
import Test.QuickCheck

instance Arbitrary Accidental where
  arbitrary = elements [DoubleFlat, Flat, Natural, Sharp, DoubleSharp]
  shrink (Offset n) =
    offset <$> if n > 0 then [0 .. (n - 1)] else [(n + 1) .. 0]

instance CoArbitrary Accidental

instance Function Accidental

--instance CoArbitrary Accidental where
--  coarbitrary (Offset n) = variant n

genAccidental :: Gen Accidental
genAccidental =
  elements [DoubleFlat, Flat, Natural, Sharp, DoubleSharp]

tests :: Spec
tests =
  describe "Accidental" $ do
    it "steps" $ do
      steps DoubleFlat `shouldBe` (-2)
      steps Natural `shouldBe` 0
      steps Sharp `shouldBe` 1
    it "mappend" $ do
      Natural <> Sharp `shouldBe` Sharp
      Flat <> Sharp `shouldBe` Natural
      DoubleSharp <> Flat `shouldBe` Sharp

specHasAccidental ::
  forall t.
  ( Eq t,
    Show t,
    Arbitrary t,
    HasAccidental t
  ) =>
  Spec
specHasAccidental =
  describe "class HasAccidental" $ do
    specInverse @t flat sharp
    prop "accidental" $ isLens (accidental :: Lens' t Accidental)

specEnharmonic ::
  forall t.
  ( Eq t,
    Show t,
    Arbitrary t,
    Enharmonic t
  ) =>
  Spec
specEnharmonic =
  describe "class Enharmonic" $ do
    --specInverse @t flatKey sharpKey
    prop "" (\(a :: t) -> flatKey a `shouldSatisfy` (not . isSharp))
    prop "" (\(a :: t) -> sharpKey a `shouldSatisfy` (not . isFlat))

    specIdempotent @t flatKey
    specIdempotent @t sharpKey
    prop "" (\(a :: t) -> steps a `octaveEq'` steps (sharpKey a))
    prop "" (\(a :: t) -> steps a `octaveEq'` steps (flatKey a))
