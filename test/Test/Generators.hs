{-# LANGUAGE FlexibleInstances #-}
module Test.Generators where

import           Music.Theory

import           Control.Lens              (has, over, set, under, (^.),
                                            _Wrapped)
import           Control.Monad.Except
import           Control.Monad.Trans.Class (lift)
import           Data.Either

import           Test.QuickCheck.Arbitrary
import           Test.QuickCheck.Gen

smallInt :: Gen Int
smallInt = chooseInt (-1000) 1000

instance Arbitrary Accidental where
  arbitrary = oneof [doubleFlat, flat, natural, sharp, doubleSharp]



-- Pitch
instance Gen Pitch where
  arbitrary = pitchFromSteps <$> chooseInt 0 11

-- Note
instance Gen Note where
  arbitrary = noteFromSteps <$> chooseInt 0 47

-- Mode
instance Gen Mode where
  arbitrary = Mode <$> genSumList 12
    where
      genSumList :: Int -> Gen (Array Int)
      genSumList n = snd <$> (evalRWST (replicateM_ (n-1) step *> end) unit 1)
          
      step :: RWST Unit (Array Int) Int Gen Unit
      step = do
        b <- lift arbitrary
        if b
          then modify_ (_+1)
          else do
            n <- get
            tell [n]
            put 1

      end :: RWST Unit (Array Int) Int Gen Unit
      end = get >>= \n -> tell [n]

-- Scale
instance Gen r => Gen (Scale r) where
  arbitrary = mkScale <$> g <*> genMode
-- Degree
genDegree :: Gen Degree
genDegree = Degree <$> genAccidental <*>chooseInt 0 12


-- Mode
genMode :: Gen Mode
genMode = Mode <$> genSumList 12
  where
    genSumList :: Int -> Gen (Array Int)
    genSumList n = snd <$> (evalRWST (replicateM_ (n-1) step *> end) unit 1)
        
    step :: RWST Unit (Array Int) Int Gen Unit
    step = do
      b <- lift arbitrary
      if b
        then modify_ (_+1)
        else do
          n <- get
          tell [n]
          put 1

    end :: RWST Unit (Array Int) Int Gen Unit
    end = get >>= \n -> tell [n]

-- Scale
genScale :: forall r. Gen r -> Gen (Scale r)
genScale g = mkScale <$> g <*> genMode


genKey :: Gen Key
genKey = do
  b <- arbitrary
  if b then key <$> genPitch else minorKey <$> genPitch

-- Chord
genChord :: forall r. Transpose r => Gen r -> Gen (Chord Identity r)
genChord g = mkChord
    <$> g
    <*> genMode
    <*> (A.nub <<< A.take 7 <$> arrayOf genDegree)

genBasicChord :: Gen BasicChord
genBasicChord = mkChord
    <$> genPitch
    <*> elements ( ionian :| [aeolian] )
    <*> (A.nub <<< A.take 7 <$> arrayOf genDegree) 

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
