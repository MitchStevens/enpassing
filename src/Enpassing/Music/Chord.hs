{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Enpassing.Music.Chord (
  Quality (..),
  Chord (..),
  mk_chord,
  to_pitches,
  ChordLike (..)
) where

import Data.List (delete, nubBy, (\\), null)
import Control.DeepSeq
import Control.Monad
import Euterpea
import Enpassing.Music.Extension
import Enpassing.Music.Interval
import Enpassing.Music.Key
import Enpassing.Music.Scale
import GHC.Generics (Generic)
import Test.QuickCheck
import Test.QuickCheck.Gen

data Quality = Maj | Min | Dom | Aug | Dim
  deriving (Eq, Generic, NFData)

data Chord = Chord PitchClass Quality [Extension]
  deriving (Generic, NFData)

instance Show Quality where
  show Maj = "maj"
  show Min = "m"
  show Dom = ""
  show Aug = "+"
  show Dim = "o"

instance ToMusic1 Chord where
  toMusic1 (Prim (Note d c)) = chord . map (Prim . Note d . f) $ to_pitches c
    where f x = (x, [])
  toMusic1 (Prim (Rest d)) = Prim (Rest d)
  toMusic1 (m1 :+: m2) = toMusic1 m1 :+: toMusic1 m2
  toMusic1 (m1 :=: m2) = toMusic1 m1 :=: toMusic1 m2
  toMusic1 (Modify c m) = Modify c (toMusic1 m)

instance Eq Chord where
  chord1 == chord2 = to_pitches chord1 `same_elements` to_pitches chord2
    where same_elements a b = null (a\\b) && null (b\\a)

instance Show Chord where
  show (Chord root qual exts) = note ++ show qual ++ concatMap show exts
    where
      note = case root of
        Cs -> "Db"
        Df -> "Db"
        Ds -> "Eb"
        Ef -> "Eb"
        Fs -> "F#"
        Gf -> "F#"
        Gs -> "Ab"
        Af -> "Ab"
        As -> "Bb"
        Bf -> "Bb"
        _ -> show root

class ChordLike c where
  convert_to   :: Keyed Chord -> Maybe (Keyed c)
  convert_from :: Keyed c -> Maybe (Keyed Chord)

instance ChordLike Chord where
  convert_to   = pure
  convert_from = pure

to_pitches :: Chord -> [Pitch]
to_pitches (Chord root qual exts) = pitch : map (scale_ext pitch (quality_mode qual)) notes
  where
    pitch = (root, 4) :: Pitch
    notes = (if contains_qual 3 then [] else [Add 3])
         ++ (if contains_qual 5 then [] else [Add 5])
         ++ exts
    contains_qual n = any (\ex -> degree ex == n) exts

quality_mode :: Quality -> Mode
quality_mode qual = case qual of
  Maj -> Major
  Min -> Minor
  Dom -> Mixolydian
  Aug -> CustomMode "Augmented"
  Dim -> CustomMode "Octatonic"

{-
  What makes a valid chord?
    - A chord cannot have a 'No x' and another modifier
    - A chord cannot have more than 7 notes
    - A chord cannot have two different extensions for the same note, i.e. Sharp 6 and a Flat 6
-}
mk_chord :: PitchClass -> Quality -> [Extension] -> Either String Chord
mk_chord note qual exts = foldM add_ext (Chord note qual []) exts

add_ext :: Chord -> Extension -> Either String Chord
add_ext (Chord note qual exts) e
  | length exts > 5 = Left "Too many notes in chord"
  | not (and degree_test)  = Left $ "Two exts of the same degree found in chord. All extensions: "++ show (implicit_extensions e ++ exts)
  | otherwise = Right $ Chord note qual (implicit_extensions e)
    where
      degree_test = diff_degree <$> implicit_extensions e <*> exts
      diff_degree a b = degree a /= degree b

-- Sometimes adding an extesion mean adding more than one extension:
--   * Cm9 is a chord with a C minor triad, a flat 7, and
implicit_extensions :: Extension -> [Extension]
implicit_extensions (Add 9)  = [Add 7, Add 9]
implicit_extensions (Add 11) = [Add 7, Add 9, Add 11]
implicit_extensions (Add 13) = [Add 7, Add 9, Add 11, Add 13]
implicit_extensions ext = [ext]
{-
instance Show Chord where
  show (Chord note qual exts) = show note ++ show qual ++ exts_str
    where exts_str = if Add 7 `elem` exts
                       then "7" ++ concatMap show (Add 7 `delete` exts)
                       else concatMap show exts
-}

