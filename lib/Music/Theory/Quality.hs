module Music.Theory.Quality
  ( Quality (..), pattern Perfect
  , ChordQuality (..)
  , ConstructQuality (constructQuality)
  , maj, perf, min, dim, aug
  )
where

import Prelude hiding (min)

data Quality = Diminished | Minor | Major | Augmented
  deriving (Eq)

pattern Perfect = Major

data ChordQuality
  = DiminishedChord 
  | MinorChord
  | MajorChord
  | AugmentedChord
  | Sus2Chord
  | Sus4Chord

instance Show Quality where
  show _ = ""

class ConstructQuality s where
  constructQuality :: Quality -> s

maj, perf, min, dim, aug :: ConstructQuality s => s
[maj, perf, min, dim, aug] =
  constructQuality <$> [Major, Perfect, Minor, Diminished, Augmented]
