module Test.Music.Theory.Transpose where

import Music.Theory
import Test.Comparators

import Test.QuickCheck
import Test.Hspec
import Text.Printf
import Control.Lens hiding (below)
import Control.Lens.Properties as Properties

tests :: IO ()
tests = hspec $ do 
  --weakTransposition @Int
  specStrongTransposition @Int

specWeakTransposition :: forall t.
                   ( Eq t
                   , Show t
                   , Arbitrary t
                   , Semitones t
                   , Transpose t) 
                  => SpecWith ()
specWeakTransposition = describe "class Transpose" $ do
  describe "Weak transposition property" $ do
    it "has a `shift` function that is commutative" $
      property propWeakShiftCommute
    it "obeys the shift simplification law" $
      property propWeakShiftSimplify
    it "`shift 0` should equal `id`" $
      property 
  specShift0NoOperation @t
  specOctaveLens @t
  where
    propWeakShiftSimplify :: Int -> t -> Property
    propWeakShiftSimplify n t = 
      steps (shift n t) `octaveEq'` (steps t + n)

    propWeakShiftCommute :: Int -> Int -> t -> Property
    propWeakShiftCommute m n t = 
      shift n (shift m t) `octaveEq'` shift (m+n) t

specStrongTransposition :: forall t. ( Eq t
                     , Show t
                     , Arbitrary t
                     , Semitones t
                     , Transpose t) 
                    => SpecWith ()
specStrongTransposition = describe "class Transpose" $ do
  describe "Strong transposition property" $ do
    it "has a `shift` function that is commutative" $
      property propStrongShiftCommute
    it "obeys the shift simplification law" $
      property propStrongShiftSimplify
  specShift0NoOperation @t
  specBelow @t
  specAbove @t
  specOctaveLens @t
  where
    propStrongShiftSimplify :: Int -> t -> Property
    propStrongShiftSimplify n t = 
      steps (shift n t) === (steps t + n)

    propStrongShiftCommute :: Int -> Int -> t -> Property
    propStrongShiftCommute m n t = 
      shift n (shift m t) === shift (m+n) t

specShift0NoOperation :: forall t.
  ( Eq t
  , Show t
  , Arbitrary t
  , Transpose t )
    => Spec
specShift0NoOperation =
  it "`shift 0` shouldBe equal to `id`" $
    property prop1
  where
    prop1 :: t -> Property
    prop1 t = shift 0 t === t

specOctaveLens :: forall t. 
  ( Eq t
  , Show t
  , Arbitrary t
  , Semitones t
  , Transpose t) 
  => SpecWith ()
specOctaveLens = describe "Octave Lens" $ do
  it "is the same octave" $
    property testSameOctave
  it "is a valid lens" $
    property (Properties.isLens @t octave)
  where
    testSameOctave :: t -> Int -> Property
    testSameOctave t n =
      (octave .~ n) t `octaveEq'` t


specBelow :: forall t. 
           ( Show t
           , Arbitrary t
           , Semitones t
           , Transpose t) 
           => SpecWith ()
specBelow = describe "`below`" $ do
  it "should create a note below `y`" $
    property prop1
  it "should create a note not more than 12 semitones above `y`" $
    property prop2
  it "should be octave equivalent to the original note" $
    property prop3
  where
    prop1, prop2, prop3 :: t -> t -> Property
    prop1 x y = 0 .< y `diff` (x `below` y)
    prop2 x y = (y `diff` (x `below` y)) .<= 12
    prop3 x y = (x `below` y) `octaveEq'` x

specAbove :: forall t.
           ( Show t
           , Arbitrary t
           , Semitones t
           , Transpose t)
          => SpecWith ()
specAbove = describe "`above`" $ do
  it "should create a note above `y`" $
    property prop1
  it "should create a note not more than 12 semitones below `y`" $
    property prop2
  it "should be octave equivalent to the original note" $
    property prop3
  where
    prop1, prop2, prop3 :: t -> t -> Property
    prop1 x y = 0 .< (x `above` y) `diff` y
    prop2 x y = (x `above` y) `diff` y .<= 12
    prop3 x y = (x `above` y) `octaveEq'` x


octaveEq' :: (Show s, Semitones s, Transpose s) 
          => s -> s -> Property
octaveEq' x y =
  counterexample (show x <> interpret res <> show y) res
    where
      res = octaveEq x y
      interpret True  = " `octaveEq` "
      interpret False = " not `octaveEq` "
