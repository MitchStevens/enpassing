module Enpassing.Music.Scale (
  scale_ext,
  degree_as_pitch,
  pitch_as_degree,
  scale_mode,
  mk_scale,
  infinite_scale,
  Scale,
  ScaleDegree (..)
) where

import           Data.Bifunctor            (second)
import           Data.List                 (elemIndex)
import           Enpassing.Music.Extension
import           Enpassing.Music.Key
import           Euterpea                  hiding (second)

type Scale = [Pitch]
data ScaleDegree = I | II | III | IV | V | VI | VII deriving (Eq, Show, Enum)

major_intervals = [w, w, h, w, w, w, h]
  where (h, w) = (1, 2)

{- Given a root note and a Keyed Extension, return whatever note is the root + the extension

-}
scale_ext :: (Pitch, Mode) -> Extension -> Pitch
scale_ext scale ext = trans accedental $ infinite_scale scale !! (degree ext - 1)
  where
    accedental = case ext of
      Sharp _ -> 1
      Add   _ -> 0
      Flat  _ -> -1

degree_as_pitch :: Keyed ScaleDegree -> PitchClass
degree_as_pitch (Keyed (root, mode) deg) = fst p
  where p = mk_scale ((root, 0), mode) !! fromEnum deg

{-  -}
pitch_as_degree :: Keyed PitchClass -> Maybe ScaleDegree
pitch_as_degree (Keyed (root, mode) note) = toEnum <$> elemIndex (pcToInt note) pitches
  where pitches = pcToInt.fst <$> mk_scale ((root, 0), mode) :: [Int]

{- Given a scale, return an infinite list of the notes in octave 1, then in octave 2, etc -}
infinite_scale :: (Pitch, Mode) -> Scale
infinite_scale key = concatMap octave [0..]
  where octave n = fmap (second (+n)) (mk_scale key)

{- Given a mode, return a scale with root starting at at the root note -}
mk_scale :: (Pitch, Mode) -> Scale
mk_scale (root, mode) = case mode of
  Major            -> scale_mode 0 root
  Minor            -> scale_mode 5 root
  Ionian           -> scale_mode 0 root
  Dorian           -> scale_mode 1 root
  Phrygian         -> scale_mode 2 root
  Lydian           -> scale_mode 3 root
  Mixolydian       -> scale_mode 4 root
  Aeolian          -> scale_mode 5 root
  Locrian          -> scale_mode 6 root
  CustomMode "Dim" -> fmap (`trans` root) [0, 2, 3, 5, 6, 8, 9, 11]
  CustomMode "Aug" -> fmap (`trans` root) [0, 3, 4, 7, 8, 11]

{- Get a mode of the major scale, 1 is major, 2 is Dorian, 3 is Phyrigian, etc. starting from a root pitch -}
scale_mode :: Int -> Pitch -> Scale
scale_mode n root = take 7 $ scanl (flip trans) root (s2 ++ s1)
  where (s1, s2) = splitAt n major_intervals
