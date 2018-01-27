module Enpassing.Changes.Interpreted where

import           Data.Maybe
import           Enpassing.Music
import           Euterpea.Music

data InterpretedChord = InterpretedChord ScaleDegree Mode [Extension] deriving (Eq, Show)

as_interpreted :: Keyed Chord -> InterpretedChord
as_interpreted crd@(Keyed k (Chord _ qual exts)) =
  fromJust $ (\deg -> InterpretedChord deg qual exts) <$> pitch_as_degree (root <$> crd)
