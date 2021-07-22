module Music.Theory.Quality where

import Prelude hiding (min)

data ChordQuality
  = Diminished
  | Minor
  | Major
  | Augmented
  | Sus2
  | Sus4

instance Show ChordQuality where
  show = \case
    Minor      -> "m"
    Major      -> "maj"
    Augmented  -> "aug"
    Diminished -> "dim"
    Sus2       -> "sus2"
    Sus4       -> "sus4"
    _          -> error "Mode Parse Error"
