module Main where

import Music.Theory
import Test.Hspec
import Test.Music.Theory.Accidental as Accidental
import Test.Music.Theory.Chord as Chord
import Test.Music.Theory.Degree as Degree
import Test.Music.Theory.Pitch as Pitch
import Test.Music.Theory.Semitones as Semitones
import Test.Music.Theory.Transpose as Transpose

main :: IO ()
main = hspec $ do
  Chord.tests
  Degree.tests
  Transpose.tests
  Pitch.tests
  Accidental.tests
  Semitones.tests
