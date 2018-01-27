module TestInterpreted where

import           Enpassing.Changes.Interpreted
import           Enpassing.Music
import           Euterpea.Music
import           Test.HUnit.Base
import           Test.HUnit.Text

main = runTestTT test >>= print
  where
    test = TestLabel "Interpreted Chord tests" $
      TestList (fmap interpreted_test conversion_tests)

conversion_tests = [
  (Keyed (C, Major) (Chord C Major []), InterpretedChord I Major []),
  (Keyed (C, Major) (Chord G Major []), InterpretedChord V Major [])]

interpreted_test :: (Keyed Chord, InterpretedChord) -> Test
interpreted_test (chord, expected) = expected ~=? actual
  where actual = as_interpreted chord

