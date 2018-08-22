{-# LANGUAGE FlexibleInstances #-}
module Test.Generators where

import           Enpassing.Theory

import           Control.Lens              (has, over, set, under, (^.),
                                            _Wrapped)
import           Control.Monad.Except
import           Control.Monad.Trans.Class (lift)
import           Data.Either

import           Test.QuickCheck.Arbitrary
import           Test.QuickCheck.Gen

instance Arbitrary Note where
  arbitrary = arbitraryBoundedEnum
  shrink = \case
    C -> []
    _ -> [C]

instance Arbitrary PitchClass where
  arbitrary = PitchClass <$> accidentalGen (arbitrary :: Gen Note)
  shrink p = PitchClass <$> accidentalShrink (p^._Wrapped)

instance Arbitrary Pitch where
  arbitrary = Pitch <$> arbitrary <*> arbitrary
  shrink (Pitch note oct) =
    Pitch <$> shrink note <*> shrink oct

accidentalGen :: Gen a -> Gen (Accidental a)
accidentalGen gen = elements [Flat, Natural, Sharp] <*> gen

accidentalShrink :: Arbitrary a => (Accidental a -> [Accidental a])
accidentalShrink = \case
  Flat    x -> Natural x : (Flat <$> shrink x)
  Natural x -> Natural <$> shrink x
  Sharp   x -> Natural x : (Sharp <$> shrink x)

instance Arbitrary (Accidental Degree) where
  arbitrary = accidentalGen arbitrary
  shrink = accidentalShrink

instance Arbitrary (Accidental (LetterCase Degree)) where
  arbitrary = accidentalGen $ elements [Upper, Lower] <*> arbitrary
  shrink = accidentalShrink

instance Arbitrary Degree where
  arbitrary = elements [d3, d5, d6, d7, d9, d11, d13]
  shrink = \case
    Degree 1 -> []
    _        -> [d1]

instance Arbitrary a => Arbitrary (LetterCase a) where
  arbitrary = elements [Upper, Lower] <*> arbitrary
  shrink = \case
    Upper x -> Lower x : (Upper <$> shrink x)
    Lower x -> Lower <$> shrink x

instance Arbitrary Mode where
  arbitrary = arbitraryBoundedEnum
  shrink = const []

instance Arbitrary Extension where
  arbitrary = oneof
    [ Acc <$> arbitrary
    , pure Sus2
    , pure Sus4
    , No <$> arbitrary ]
    where  
  shrink = const []

eitherFold :: (a -> b -> Either e a) -> a -> [b] -> a
eitherFold f a list = case list of
  b:bs -> case f a b of
    Left  e  -> a
    Right a' -> eitherFold f a' bs
  []   -> a

fromGenEither :: Gen (Either e a) -> Gen a
fromGenEither genE = fromRight undefined
  <$> genE `suchThat` isRight

genExtensions :: Gen [Extension]
genExtensions = resize 7 (listOf arbitrary)

chordShrink :: ExtendedClass c => c -> [c]
chordShrink c = [set exts [] c | not.null $ (c^.exts)]

instance Arbitrary ExtendedChord where
  arbitrary = do
    chord <- extended <$> arbitrary <*> elements chordModes <*> pure [] -- Either e Chord
    exts <- infiniteListOf arbitrary
    fromGenEither . pure $ eitherFold addExtension <$> chord <*> pure exts
  shrink = const []

instance Arbitrary InterpretedChord where
  arbitrary = do
    chord <- interpreted <$> degreeGen <*> elements chordModes <*> pure []
    exts <- infiniteListOf arbitrary
    fromGenEither . pure $ eitherFold addExtension <$> chord <*> pure exts
    where
      degreeGen = accidentalGen (elements [d1, d2, d3, d4, d5, d6, d7])
  shrink = const []

instance Arbitrary SlashChord where
  arbitrary = fromGenEither $ slash <$> arbitrary <*> arbitrary
  shrink = const []

instance Arbitrary Chord where
  arbitrary = oneof
    [ Extended    <$> arbitrary
    , Interpreted <$> arbitrary
    , Slash       <$> arbitrary ]
  shrink = const []
