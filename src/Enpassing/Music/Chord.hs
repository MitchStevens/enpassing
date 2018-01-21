{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Enpassing.Music.Chord (
  Quality (..),
  Chord (..),
  mk_chord,
  to_pitches,
  transpose_chord
) where

import           Control.DeepSeq
import           Control.Monad
import           Data.Function             (on)
import           Data.List                 (delete, nubBy, null, (\\))
import           Enpassing.Music.Extension
import           Enpassing.Music.Interval
import           Enpassing.Music.Key
import           Enpassing.Music.Scale
import           Euterpea                  hiding (transpose)
import           GHC.Generics              (Generic)
import           Test.QuickCheck
import           Test.QuickCheck.Gen

{-|
  The quality of a Chord is then
    Maj - Root, Maj3, Per5 and maybe Maj7
    Min - Root, Min3, Per5 and maybe Min7
    Dom - Root, Maj3, Per5 and maybe Min7
    Aug - Root, Maj3, Min6 and maybe Min7
    Dim - Root, Min3, Tritone, Maj6

    The quality of a half diminished chord (say, Bm7b5) would be represented as Chord B Min [Add 7, Flat 5], there is no special quality to represent it.
|-}
data Quality = Maj | Min | Dom | Aug | Dim
  deriving (Eq, Generic, NFData, Bounded)

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

{-| Two chords are considered 'equal' if they have the same notes. Under this eqiuvalence, a Bm7(b5) (B, D, F, A) and Dm6 (D, F, A, B) would be equal. This might cause some problems in the future, one possible change could be to also check that the root chord is the same. |-}
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
        _  -> show root

{-
  Converting a chord to a collection of pitches is non-trivial. THings to keep in mind:
        - The root is the lowest note
        -scale_ext :: Pitch -> Mode -> Extension -> Pitch
-}
to_pitches :: Chord -> [Pitch]
to_pitches (Chord root qual exts) = root_note : map (scale_ext root_note (quality_mode qual)) notes
  where
    root_note = (root, 4) :: Pitch
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
implicit_extensions ext      = [ext]

transpose_chord :: Int -> Chord -> Chord
transpose_chord n (Chord root qual exts) = Chord (fst . pitch . (+1) . pcToInt $ root) qual exts


--Generators for chords
instance Arbitrary Quality where
  arbitrary = arbitraryBoundedEnum

  shrink Major = []
  shrink x     = [Major]

instance Arbitrary Chord where
  arbitrary = do
    root <- arbitrary
    qual <- arbitrary
    exts <- do
      n <- choose (0, 7)
      e <- vectorOf len
      return $ nubBy ((==) `on` degree) e
    return $ Chord root qual exts
