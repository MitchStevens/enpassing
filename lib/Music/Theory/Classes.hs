module Music.Theory.Classes where

import Control.Lens
import Music.Theory.Accidental
import Music.Theory.Interval
import Music.Theory.Note
import Music.Theory.Transpose

class HasRoot s a | s -> a where
  root :: Lens' s a

class HasIntervals s where
  intervals :: Lens' s [Interval]

class HasPitchName s where
  pitchName :: Lens' s NoteName

class HasOctave s a | s -> a where
  pitchOctave :: Lens' s a

arpeggiate :: (HasRoot t a, HasIntervals t, Transpose a) => t -> [a]
arpeggiate t = (`shift` (t ^. root)) <$> (t ^. intervals)
