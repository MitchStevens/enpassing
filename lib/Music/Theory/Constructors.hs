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

constructChord :: Note -> Maybe Accidental -> Maybe Quality -> [Interval] -> Chord
constructChord note acc quality exts =
  let acc' = fromMaybe Natural acc
      quality' = fromMaybe Major quality
  in
    newChord (PitchClass note acc') (triad quality' <> exts)

instance Constructor (Accidental -> Quality -> [Interval] -> Chord) where
  noteConstruct note acc qual exts = constructChord note (Just acc) (Just qual) exts
instance Constructor (              Quality -> [Interval] -> Chord) where
  noteConstruct note qual exts = constructChord note Nothing (Just qual) exts
instance Constructor (Accidental ->            [Interval] -> Chord) where
  noteConstruct note acc exts = constructChord note (Just acc) Nothing exts
instance Constructor (                         [Interval] -> Chord) where
  noteConstruct note exts = constructChord note Nothing Nothing exts
instance Constructor (Accidental -> Quality ->               Chord) where
  noteConstruct note acc qual = constructChord note (Just acc) (Just qual) []
instance Constructor (              Quality ->               Chord) where
  noteConstruct note qual = constructChord note Nothing (Just qual) []
instance Constructor (Accidental ->                          Chord) where
  noteConstruct note acc = constructChord note (Just acc) Nothing []

c, d, e, f, g, a, b :: Constructor c => c
c = noteConstruct C
d = noteConstruct D
e = noteConstruct E
f = noteConstruct F
g = noteConstruct G
a = noteConstruct A
b = noteConstruct B
