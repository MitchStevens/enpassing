module TestChord where

import           Control.Monad
import           Data.List       ((\\))
import qualified Data.Text       as T
import           Enpassing.Music
import           Euterpea
import           Parsers
import           Test.HUnit.Base
import           Test.HUnit.Text
import           Text.Printf

main :: IO ()
main = void $ runTestTT all_tests

[c4,cs4,d4,ds4,e4,f4,fs4,g4,gs4,a4,as4,b4,c5,cs5,d5,ds5,e5,f5,fs5,g5,gs5,a5,as5,b5] = map (\x -> trans x (C, 4)) [0..23]

all_tests = TestLabel "Chord Tests" $ TestList [
  TestLabel "Major Tests" $ TestList (map chord_test major_tests),
  TestLabel "Minor Tests" $ TestList (map chord_test minor_tests),
  TestLabel "Diminished Tests" $ TestList (map chord_test dim_tests)]

major_tests = [
  (Chord C Major [],             [c4, e4, g4]),
  (Chord C Major [Add 7],        [c4, e4, g4, b4]),
  (Chord C Major [Add 7, Add 9], [c4, e4, g4, b4, d5]),
  (Chord D Major [Add 7],        [d4, fs4, a4, cs5]),
  (Chord E Major [Add 7],        [e4, gs4, b4, ds5]),
  (Chord F Major [Add 7],        [f4, a4, c5, e5]),
  (Chord G Major [Add 7],        [g4, b4, d5, fs5]),
  (Chord D Major [Add 7, Add 9, Add 11, Add 13], [d4, fs4, a4, cs5, e5, g5, b5])]

minor_tests = [
  (Chord A  Minor [Add 7],         [a4, c5, e5, g5]),
  (Chord G  Minor [Add 7],         [g4, as4, d5, f5]),
  (Chord C  Minor [Add 7],         [c4, ds4, g4, as4]),
  (Chord Fs Minor [Add 7, Add 9],  [fs4, a4, cs5, e5, gs5]),
  (Chord B  Minor [Add 7, Flat 5], [b4, d5, f5, a5]),
  (Chord Ds Minor [Add 7, Add 11, Flat 5], [ds4, fs4, a4, cs5, gs5])]

dim_tests = [
  (Chord A  dim [Add 7],       [a4, c5, ds5, fs5]),
  (Chord As dim [Add 7],       [as4, cs5, e5, g5]),
  (Chord B  dim [Add 7],       [b4, d5, f5, gs5])]
  where dim = CustomMode "Dim"

chord_test :: (Chord, [Pitch]) -> Test
chord_test (chord, expected) = TestCase $ assertBool message condition
  where
    message = printf "Testing chord %s:\n * Expected:    %s\n * Acually got: %s"
      (show chord)
      (show expected)
      (show actual)
    condition = same_elements expected actual
    actual = to_pitches chord

same_elements :: Eq a => [a] -> [a] -> Bool
same_elements x y = null (x \\ y) && null (y \\ x)
