module Test.Music.Theory.Transpose
  ( tests,
    specSemitones,
    specStrongTransposition,
    specWeakTransposition,
  )
where

import Control.Lens hiding (below)
import Control.Lens.Properties (isLens)
import Music.Theory
import Test.Function
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.Music.Theory.Interval hiding (tests)
import Test.Music.Theory.Semitones hiding (tests)
import Test.QuickCheck
import Text.Printf

tests :: Spec
tests = describe "Transpose" $ do
  it "setOctave" $ do
    setOctave (0 :: Int) 0 `shouldBe` 0
    setOctave (0 :: Int) 1 `shouldBe` 12
    setOctave (1 :: Int) 0 `shouldBe` 1
    setOctave (1 :: Int) 1 `shouldBe` 13
    setOctave (1 :: Int) 3 `shouldBe` 37
  specWeakTransposition @Int
  specStrongTransposition @Int

--specTransposition :: forall t.
--  ( Semitones t
--  , Transpose t )
--  => Spec
--specTransposition zero =
--  prop "" $
--    prop_Group zero (\a b -> transpose (steps b) a)

specWeakTransposition ::
  forall t.
  ( Eq t,
    Show t,
    --Typeable t,
    Arbitrary t,
    Semitones t,
    Transpose t
  ) =>
  Spec
specWeakTransposition = describe ("Weak transposition property") $ do
  prop "has a `shift` function that is commutative" propWeakShiftCommute
  prop "obeys the shift simplification law" propWeakShiftSimplify
  specShift0NoOperation @t
  where
    propWeakShiftSimplify :: Interval -> t -> Property
    propWeakShiftSimplify n t =
      steps (shift n t) `octaveEq'` (steps t + steps n)

    propWeakShiftCommute :: Interval -> Interval -> t -> Property
    propWeakShiftCommute m n t =
      shift n (shift m t) `octaveEq'` shift (m <> n) t

specStrongTransposition ::
  forall t.
  ( Eq t,
    Show t,
    Arbitrary t,
    Semitones t,
    Transpose t
  ) =>
  Spec
specStrongTransposition = describe "class Transpose" $ do
  describe "Strong transposition property" $ do
    prop "has a `shift` function that is commutative" propStrongShiftCommute
    prop "obeys the shift simplification law" propStrongShiftSimplify
  specShift0NoOperation @t
  --specBelow @t
  --specAbove @t
  specOctaveLens @t
  where
    propStrongShiftSimplify :: Interval -> t -> Property
    propStrongShiftSimplify n t =
      steps (shift n t) === (steps t + steps n)

    propStrongShiftCommute :: Interval -> Interval -> t -> Property
    propStrongShiftCommute m n t =
      shift n (shift m t) === shift (m <> n) t

specShift0NoOperation ::
  forall t.
  ( Eq t,
    Show t,
    Arbitrary t,
    Transpose t
  ) =>
  Spec
specShift0NoOperation =
  prop "`shift 0` shouldBe equal to `id`" prop1
  where
    prop1 :: t -> Property
    prop1 t = shift "P1" t === t

specOctaveLens ::
  forall t.
  ( Eq t,
    Show t,
    Arbitrary t,
    Semitones t,
    Transpose t
  ) =>
  Spec
specOctaveLens = describe "Octave Lens" $ do
  prop "is the same octave" testSameOctave
  prop "isLens" $ isLens (octave :: Lens' t Int)
  where
    testSameOctave :: t -> Int -> Property
    testSameOctave t n =
      --trace (show t <> ", n = " <> show n) $
      --trace (show $ (octave .~ n) t) $
      (octave .~ n) t `octaveEq'` t

--specBelow :: forall t.
--  ( Show t
--  , Arbitrary t
--  , Semitones t
--  , Transpose t)
--  => Spec
--specBelow = describe "`below`" $ do
--  prop "should create a note below `y`" prop1
--  prop "should create a note not more than 12 semitones above `y`" prop2
--  prop "should be octave equivalent to the original note" prop3
--  where
--    prop1, prop2 :: t -> t -> Bool
--    prop1 x y = y `diff` (x `below` y) > 0
--    prop2 x y = y `diff` (x `below` y) <= 12
--
--    prop3 :: t -> t -> Property
--    prop3 x y = (x `below` y) `octaveEq'` x
--
--specAbove :: forall t.
--           ( Show t
--           , Arbitrary t
--           , Semitones t
--           , Transpose t)
--          => Spec
--specAbove = describe "`above`" $ do
--  prop "should create a note above `y`" prop1
--  prop "should create a note not more than 12 semitones below `y`" prop2
--  prop "should be octave equivalent to the original note" prop3
--  where
--    prop1, prop2 :: t -> t -> Bool
--    prop1 x y = (x `above` y) `diff` y > 0
--    prop2 x y = (x `above` y) `diff` y <= 12
--
--    prop3 :: t -> t -> Property
--    prop3 x y = (x `above` y) `octaveEq'` x

-- Camparators (do not export!)
--semitoneBinaryProperty :: (Arbitrary t, Show t, Semitones t)
--                       => (Int -> Int -> Bool) -> String -> t -> t -> Property
--semitoneBinaryProperty op opName a b =
--  counterexample (printf "%s %s %s should be True" (show a) opName (show b)) (op (steps a) (steps b))

--(.==), (./=), (.>), (.>=), (.<), (.<=), octaveEq' :: (Arbitrary t, Show t, Semitones t) => t -> t -> Property
--(.==) = semitoneBinaryProperty (==) "=="
--(./=) = semitoneBinaryProperty (/=) "/="
--(.>)  = semitoneBinaryProperty (>)  ">"
--(.>=) = semitoneBinaryProperty (>=) ">="
--(.<)  = semitoneBinaryProperty (<)  "<"
--(.<=) = semitoneBinaryProperty (<=) "<="
