module Test.Music.Theory.Transpose
  ( tests
  , specSemitones
  , specStrongTransposition
  , specWeakTransposition
) where

import Data.Validity
import Test.Validity.Optics
import Test.QuickCheck
import Data.GenValidity
import Test.Hspec
import Test.Hspec.QuickCheck
import Data.Proxy
import Data.Typeable

import Text.Printf
import Control.Lens hiding (below)
import Control.Lens.Properties as Properties

import Music.Theory
import Test.Function

tests :: Spec
tests = describe "Transpose" $ do 
  specWeakTransposition @Int
  specStrongTransposition @Int


specSemitones :: forall t.
  ( Eq t
  , Show t
  , Arbitrary t
  , Semitones t )
  => Spec
specSemitones = specRelation @t (\a b -> mod12 (steps a) == mod12 (steps b))

--specTransposition :: forall t.
--  ( Semitones t
--  , Transpose t )
--  => Spec
--specTransposition zero =
--  prop "" $
--    prop_Group zero (\a b -> transpose (steps b) a)


specWeakTransposition :: forall t.
  ( Eq t
  , Show t
  , Typeable t
  , Arbitrary t
  , Semitones t
  , Transpose t) 
  => Spec
specWeakTransposition = describe ("Weak transposition property of " <> ty) $ do
  prop "has a `shift` function that is commutative" propWeakShiftCommute
  prop "obeys the shift simplification law" propWeakShiftSimplify
  specShift0NoOperation @t
  --specOctaveLens @t 
  where
    ty = show $ typeOf (Proxy :: Proxy t)

    propWeakShiftSimplify :: Int -> t -> Property
    propWeakShiftSimplify n t = 
      steps (transpose n t) `octaveEq'` (steps t + n)

    propWeakShiftCommute :: Int -> Int -> t -> Property
    propWeakShiftCommute m n t = 
      transpose n (transpose m t) `octaveEq'` transpose (m+n) t

specStrongTransposition :: forall t.
  ( Eq t
  , Show t
  , Arbitrary t
  , Semitones t
  , Transpose t) 
  => Spec
specStrongTransposition = describe "class Transpose" $ do
  describe "Strong transposition property" $ do
    prop "has a `shift` function that is commutative" propStrongShiftCommute
    prop "obeys the shift simplification law" propStrongShiftSimplify
  specShift0NoOperation @t
  specBelow @t
  specAbove @t
  specOctaveLens @t
  where
    propStrongShiftSimplify :: Int -> t -> Property
    propStrongShiftSimplify n t = 
      steps (transpose n t) === (steps t + n)

    propStrongShiftCommute :: Int -> Int -> t -> Property
    propStrongShiftCommute m n t = 
      transpose n (transpose m t) === transpose (m+n) t

specShift0NoOperation :: forall t.
  ( Eq t
  , Show t
  , Arbitrary t
  , Transpose t )
  => Spec
specShift0NoOperation =
  prop "`shift 0` shouldBe equal to `id`" prop1
  where
    prop1 :: t -> Property
    prop1 t = shift 0 t === t

specOctaveLens :: forall t. 
  ( Eq t
  , Show t
  , Arbitrary t
  , Semitones t
  , Transpose t) 
  => Spec
specOctaveLens = describe "Octave Lens" $ do
  prop "is the same octave" testSameOctave
  lensSpecOnArbitrary (octave :: Lens' t Int)
  where
    testSameOctave :: t -> Int -> Property
    testSameOctave t n =
      (octave .~ n) t `octaveEq'` t

specBelow :: forall t. 
  ( Show t
  , Arbitrary t
  , Semitones t
  , Transpose t) 
  => Spec
specBelow = describe "`below`" $ do
  prop "should create a note below `y`" prop1
  prop "should create a note not more than 12 semitones above `y`" prop2
  prop "should be octave equivalent to the original note" prop3
  where
    prop1, prop2 :: t -> t -> Bool
    prop1 x y = y `diff` (x `below` y) > 0
    prop2 x y = y `diff` (x `below` y) <= 12
    
    prop3 :: t -> t -> Property
    prop3 x y = (x `below` y) `octaveEq'` x

specAbove :: forall t.
           ( Show t
           , Arbitrary t
           , Semitones t
           , Transpose t)
          => Spec
specAbove = describe "`above`" $ do
  prop "should create a note above `y`" prop1
  prop "should create a note not more than 12 semitones below `y`" prop2
  prop "should be octave equivalent to the original note" prop3
  where
    prop1, prop2 :: t -> t -> Bool
    prop1 x y = (x `above` y) `diff` y > 0
    prop2 x y = (x `above` y) `diff` y <= 12

    prop3 :: t -> t -> Property
    prop3 x y = (x `above` y) `octaveEq'` x


-- Camparators (do not export!)
semitoneBinaryProperty :: (Arbitrary t, Show t, Semitones t)
                       => (Int -> Int -> Bool) -> String -> t -> t -> Property
semitoneBinaryProperty op opName a b =
  counterexample (printf "%s %s %s should be True" (show a) opName (show b)) (op (steps a) (steps b))


(.==), (./=), (.>), (.>=), (.<), (.<=), octaveEq' :: (Arbitrary t, Show t, Semitones t) => t -> t -> Property
(.==) = semitoneBinaryProperty (==) "==" 
(./=) = semitoneBinaryProperty (/=) "/=" 
(.>)  = semitoneBinaryProperty (>)  ">" 
(.>=) = semitoneBinaryProperty (>=) ">=" 
(.<)  = semitoneBinaryProperty (<)  "<" 
(.<=) = semitoneBinaryProperty (<=) "<="
  
octaveEq' a b = 
  counterexample (printf "octaveEq %s %s should be True" (show a) (show b)) (octaveEq a b)

