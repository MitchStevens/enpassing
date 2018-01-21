module TestInterpreted (tests) where

import           Enpassing.Changes.Interpreted
import           Enpassing.Music
import           Euterpea.Music
import           Test.Tasty
import           Test.Tasty.HUnit

tests = conversion_tests

conversion_tests = let cases = [(Keyed (C, Major) (Chord C Maj []), InterpretedChord I Maj [])]
                   in testGroup "Conversion" $ map interpreted_test cases

interpreted_test :: (Keyed Chord, InterpretedChord) -> TestTree
interpreted_test (chord, interpreted) = testCase "Interpreted Test" $ assertBool "" bool
    where bool = interpreted == as_interpreted chord

