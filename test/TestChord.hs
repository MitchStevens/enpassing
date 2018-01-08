module TestChord (tests) where

import Parsers
import Enpassing.Music
import Euterpea
import qualified Data.Text as T
import Data.List ((\\))
import Test.Tasty
import Test.Tasty.HUnit

tests = test_to_pitches

[c4,cs4,d4,ds4,e4,f4,fs4,g4,gs4,a4,as4,b4,c5,cs5,d5,ds5,e5,f5,fs5,g5,gs5,a5,as5,b5] = map (\x -> trans x (C, 4)) [0..23]

test_to_pitches = testGroup "Testing the pitches in a Chord" [major_tests, minor_tests, dim_tests]

major_tests = let cases = [(Chord C Maj [],             [c4, e4, g4]),
                           (Chord C Maj [Add 7],        [c4, e4, g4, b4]),
                           (Chord C Maj [Add 7, Add 9], [c4, e4, g4, b4, d5]),
                           (Chord D Maj [Add 7],        [d4, fs4, a4, cs5]),
                           (Chord E Maj [Add 7],        [e4, gs4, b4, ds5]),
                           (Chord F Maj [Add 7],        [f4, a4, c5, e5]),
                           (Chord G Maj [Add 7],        [g4, b4, d5, fs5]),
                           (Chord D Maj [Add 7, Add 9, Add 11, Add 13], [d4, fs4, a4, cs5, e5, g5, b5])]
              in testGroup "Major Chords" $ map chord_test cases

minor_tests = let cases = [(Chord A  Min [Add 7],         [a4, c5, e5, g5]),
                           (Chord G  Min [Add 7],         [g4, as4, d5, f5]),
                           (Chord C  Min [Add 7],         [c4, ds4, g4, as4]),
                           (Chord Fs Min [Add 7, Add 9],  [fs4, a4, cs5, e5, gs5]),
                           (Chord B  Min [Add 7, Flat 5], [b4, d5, f5, a5]),
                           (Chord Ds Min [Add 7, Add 11, Flat 5], [ds4, fs4, a4, cs5, gs5])]
              in testGroup "Minor Chords" $ map chord_test cases

dim_tests = let cases = [(Chord A  Dim [Add 7],       [a4, c5, ds5, fs5]),
                         (Chord As Dim [Add 7],       [as4, cs5, e5, g5]),
                         (Chord B  Dim [Add 7],       [b4, d5, f5, gs5])]
            in testGroup "Diminished Chords" $ map chord_test cases

chord_test :: (Chord, [Pitch]) -> TestTree
chord_test (chord, pitches) = testCase message $ assertBool "" (same_elements pitches (to_pitches chord))
  where
    message = "A chord "++ show chord ++ " should produce the pitches "++ show pitches
    same_elements a b = null (a\\b) && null (b\\a)
