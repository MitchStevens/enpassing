module Enpassing.Music.Interpreted where

import Enpassing.Music.Chord
import Enpassing.Music.Extension
import Enpassing.Music.Key
import Enpassing.Music.Scale

data InterpretedChord = InterpretedChord ScaleDegree Quality [Extension]

instance ChordLike InterpretedChord where
  convert_to (Keyed k (Chord root qual exts)) =
    (\deg -> Keyed k (InterpretedChord deg qual exts)) <$> scale_degree k root

  convert_from (Keyed k (InterpretedChord deg qual exts)) =
    Just $ Keyed k $ Chord (scale_notes k !! fromEnum deg) qual exts