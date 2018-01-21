module Enpassing.Changes.Interpreted where

import           Data.Maybe
import           Enpassing.Music

data InterpretedChord = InterpretedChord ScaleDegree Quality [Extension] deriving (Eq)

as_interpreted :: Keyed Chord -> InterpretedChord
as_interpreted (Keyed k (Chord root qual exts)) =
  fromJust $ (\deg -> InterpretedChord deg qual exts) <$> scale_degree k root
