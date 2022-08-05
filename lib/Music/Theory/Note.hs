module Music.Theory.Note where

import GHC.Generics
import Music.Theory.Semitones

data NoteName = C | D | E | F | G | A | B
  deriving (Eq, Show, Enum, Bounded, Generic)

instance Semitones NoteName where
  steps n = case n of
    C -> 0
    D -> 2
    E -> 4
    F -> 5
    G -> 7
    A -> 9
    B -> 11
