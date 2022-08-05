-- |

module KeyCenter where

import Music.Theory
import Progression

type Key = PitchClass
data ChordAnalysis crd = ChordContext
  { chord :: crd
  , }



analyseChord :: Chord PitchClass -> Reader Key (Chord (Accidental, Degree))
analyseChord key chord = mod12 (steps (chord^.root) - steps key)

{-
  A chord `c` is in the key of `k` if
    1.
-}
inKey :: Chord PitchClass -> Reader Key Bool
inKey =

keyFit :: Chord PitchClass -> Reader Key Double
keyFit =

{-
Progression (Chord a) -> Progression (Chord Interval)
Progression (Chord Interval) -> Progression (Map Key Double)
Progression (Map Key (Chord j)) -> Progression (Map Key Double)
Progression (Map Key Double) -> [(Key, Progression (Chord Degree))]
-}

Progression (Map Key )
