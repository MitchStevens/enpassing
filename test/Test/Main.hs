module Main where

import Test.Hspec

import Music.Theory
import Test.Music.Theory.Degree    as Degree
--import Test.Music.Theory.Chord     as Chord
import Test.Music.Theory.Pitch     as Pitch
import Test.Music.Theory.Transpose as Transpose

main :: IO ()
main = hspec $ do
  Degree.tests
  Transpose.tests
  Pitch.tests
--  Chord.tests
--  pure ()

{-
import           Control.Monad
import           Data.Proxy
import           System.Environment
import qualified TestChord
--import           TestInterpreted
import qualified PropInterpreted
import qualified PropScale
import qualified TestInterpreted
import           TestParsers
import           TestScale
import qualified TestSheet

main = do
  TestChord.main
  TestInterpreted.main

  PropInterpreted.main
  PropScale.main


non_io_tests :: [TestTree]
non_io_tests = [
  TestParsers.tests,
  TestChord.tests,
  TestScale.tests,
  TestInterpreted.tests]

io_tests :: IO [TestTree]
io_tests = sequence [
  TestSheet.tests]
-}
