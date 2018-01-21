module Enpassing.Music.Scale (
  scale_ext,
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

{- Given a root note and a Keyed Extension, return whatever note is the root + the extension
  Examples:
    scale_ext (C, 0) (Keyed (C, Major) (Add 7)) = (B, 0)
    scale_ext (C, 0) (Keyed (G, Mox..) (Add 7)) = (B, 0)
    scale_ext (C, 0) (Keyed (C, Mix..) (Add 7)) = (Bb, 0)
    scale_ext (G, 1) (Keyed (G))
-}
scale_ext :: Pitch -> Keyed Extension -> Pitch
scale_ext root (Keyed k ext) = pitch $ (cycle_scale scale !! (degree ext - 1)) + accedental
  where
    accedental = case ext of
      Sharp _ -> 1
      Add   _ -> 0
      Flat  _ -> -1

{-  -}
scale_degree :: Key -> Maybe ScaleDegree
scale_degree (root, mode) = toEnum . fromEnum <$> elemIndex note (scale_notes key)

{- Given a scale, return an infinite list of the notes in octave 1, then in octave 2, etc -}
cycle_scale :: Key -> Scale
cycle_scale key = concatMap (\x -> fmap (+x) (mk_scale key)) [0, 12..]

{- Given a mode, return a scale with root starting at at the root note -}
mk_scale :: Key -> Scale
mk_scale (root, mode) = fmap (root+) $ case mode of
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
