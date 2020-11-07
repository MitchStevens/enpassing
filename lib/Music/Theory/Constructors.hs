{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
module Music.Theory.Constructors where

import Music.Theory.Accidental
import Music.Theory.Chord
import Music.Theory.Interval
import Music.Theory.Quality
import Music.Theory.Pitch
import Music.Theory.Scale

class Constructor c where
  noteConstruct :: Note -> c

instance Constructor (PitchClass) where
  noteConstruct note = natural note
instance Constructor (Accidental -> PitchClass) where
  noteConstruct note acc = constructAcc acc note

-- instance Constructor (Int -> Pitch) where
--   noteConstruct note octave = Pitch (natural note) octave
-- instance Constructor (Accidental -> Int -> Pitch) where
--   noteConstruct note accoctave = Pitch (constructAcc acc note) octave

{-
scale
-}

{-
chord
c [sharp] major [seventh]
-}
instance Constructor (Quality -> Chord) where
  noteConstruct note quality = newChord (PitchClass note Natural) (triad quality)

c, d, e, f, g, a, b :: Constructor c => c
c = noteConstruct C
d = noteConstruct D
e = noteConstruct E
f = noteConstruct F
g = noteConstruct G
a = noteConstruct A
b = noteConstruct B
