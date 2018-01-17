module Enpassing.Changes.ChordLike where

import           Control.Monad
import           Data.Maybe
import           Enpassing.Changes.Interpreted
import           Enpassing.Music

{-
    A datatype is 'chord-like' if it can possibly be converted into a chord.
-}
class ChordLike c where
  safe_convert_to :: Keyed Chord -> Maybe c

  convert_to :: Keyed Chord -> c
  convert_to = fromJust . safe_convert_to

  convert_from :: Keyed c -> Chord

  cmap :: (Keyed c -> c) -> Keyed Chord -> Chord
  cmap f  = extend convert_from . extend f . extend convert_to

{-| A chord is trivially chord-like |-}
instance ChordLike Chord where
  can_convert_to   = Just . extract
  convert_to       = extract
  convert_from     = extract

{-| An InterpretedChord is chord-like |-}
instance ChordLike InterpretedChord where
  safe_convert_to (Keyed k (Chord root qual exts)) =
    (\deg -> InterpretedChord deg qual exts) <$> scale_degree k root

  convert_from (Keyed k (InterpretedChord deg qual exts)) =
    Chord (scale_notes k !! fromEnum deg) qual exts
