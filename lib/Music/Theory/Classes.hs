module Music.Theory.Classes where

import Control.Lens

import Music.Theory.Interval
import Music.Theory.Transpose

class HasRoot s a | s -> a where
  root :: Lens' s a

class HasIntervals s where
  intervals :: Lens' s [Interval]

arpeggiate :: (HasRoot t a, HasIntervals t, Transpose a) => t -> [a]
arpeggiate t = (`shift` (t^.root)) <$> (t^.intervals)
