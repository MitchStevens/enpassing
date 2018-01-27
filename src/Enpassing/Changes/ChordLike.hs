module Enpassing.Changes.ChordLike where

import           Control.Comonad
import           Control.Monad
import           Data.Maybe
import           Enpassing.Changes.Interpreted
import           Enpassing.Music

{-
    A datatype is 'chord-like' if it can possibly be converted into a chord.
-}
class ChordLike c where
  as_chord :: Keyed c -> Chord

instance ChordLike Chord where
  as_chord = extract

instance ChordLike InterpretedChord where
 as_chord (Keyed k (InterpretedChord root qual exts)) =
  Chord (degree_as_pitch (Keyed k root)) qual exts

  --mk_scale :: (Pitch, Mode) -> Scale
