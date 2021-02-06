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
  = MinorChord
  | DominantChord
  | MajorChord
  | AugmentedChord
  | DiminishedChord
  | Sus2Chord
  | Sus4Chord

instance Show Quality where
  show = \case
    Diminished -> "dim"
    Minor -> "min"
    Major -> "maj"
    Augmented -> "aug"


class ConstructQuality s where
  constructQuality :: Quality -> s

maj, perf, min, dim, aug :: ConstructQuality s => s
[maj, perf, min, dim, aug] =
  constructQuality <$> [Major, Perfect, Minor, Diminished, Augmented]
