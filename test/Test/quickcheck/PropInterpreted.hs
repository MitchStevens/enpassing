module PropInterpreted where

import           Control.Comonad
import           Enpassing.Changes
import           Enpassing.Changes.Predicate
import           Enpassing.Music
import           Test.QuickCheck
import           Text.Printf

main :: IO ()
main = do
  putStrLn "\nPropInterpreted tests"
  quickCheck conversion_test

conversion_test :: Keyed Chord -> Property
conversion_test kc = counterexample message $
  is_interpreted kc ==>
  (conv kc == extract kc)
  where
    message = printf "Chord Conversion Test Failed\n\
    \\t* Chord -> InterpretedChord -> Chord\n\
    \\t* %s -> %s -> %s\n\
    \Notes in expected: %s\n\
    \Notes in actual:   %s"
      (show $ extract kc)
      (show $ as_interpreted kc)
      (show $ conv kc)
      (show $ to_pitches (extract kc))
      (show $ to_pitches (conv kc))

    conv :: Keyed Chord -> Chord
    conv = as_interpreted =>= as_chord
