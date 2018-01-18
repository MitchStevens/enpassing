module Enpassing.Music.Scale (
  scale_ext,
  subscale,
  scale_degree,
  scale_notes,
  ScaleDegree (..)
) where

import           Data.List                 (elemIndex)
import           Enpassing.Music.Extension
import           Enpassing.Music.Key
import           Euterpea

type Scale = [AbsPitch]
data ScaleDegree = I | II | III | IV | V | VI | VII deriving (Eq, Show, Enum)

major_intervals = [w, w, h, w, w, w, h]
  where (h, w) = (1, 2)

scale_ext :: Pitch -> Mode -> Extension -> Pitch
scale_ext root mode ext = pitch $ (inf_scale (mk_scale mode) !! (degree ext - 1)) + accedental + absPitch root
  where
    accedental = case ext of
      Sharp _ -> 1
      Add   _ -> 0
      Flat  _ -> -1

scale_degree :: Key -> PitchClass -> Maybe ScaleDegree
scale_degree key note = toEnum. fromEnum <$> elemIndex note (scale_notes key)

scale_notes :: Key -> [PitchClass]
scale_notes (root, mode) = fst . pitch . (r+) <$> mk_scale mode
  where r = fromEnum root

subscale :: Pitch -> Mode -> Int -> [Pitch]
subscale root mode n = take n . map (pitch . (+ absPitch root)) $ mk_scale mode

inf_scale :: Scale -> Scale
inf_scale pitches = [0, 12..] >>= (\x -> (+x) <$> take 7 pitches)

mk_scale :: Mode -> Scale
mk_scale mode = case mode of
  Major                  -> shift_scale 0
  Minor                  -> shift_scale 5
  Ionian                 -> shift_scale 0
  Dorian                 -> shift_scale 1
  Phrygian               -> shift_scale 2
  Lydian                 -> shift_scale 3
  Mixolydian             -> shift_scale 4
  Aeolian                -> shift_scale 5
  Locrian                -> shift_scale 6
  CustomMode "Augmented" -> [0, 3, 4, 7, 8, 11]
  CustomMode "Octatonic" -> [0, 2, 3, 5, 6, 8, 9, 11]
  CustomMode _           -> undefined

shift_scale :: Int -> [AbsPitch]
shift_scale n = scanl (+) 0 (s2 ++ s1)
  where (s1, s2) = splitAt n major_intervals
