module Enpassing.Changes.ChordPredicate where

import Enpassing.Music
import Euterpea.Music

type ChordPredicate = Chord -> Bool

true :: Chord -> Bool
true = const True

false :: Chord -> Bool
false = const False

has_root :: PitchClass -> ChordPredicate
has_root p1 (Chord p2 _ _) = p1 == p2

has_qual :: Quality -> ChordPredicate
has_qual q1 (Chord _ q2 _) = q1 == q2