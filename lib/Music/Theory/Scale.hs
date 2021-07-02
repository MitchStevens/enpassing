{-# LANGUAGE OverlappingInstances #-}
module Music.Theory.Scale where

import Control.Lens
import Data.Functor

import Music.Theory.Accidental
import Music.Theory.Degree
import Music.Theory.Quality
import Music.Theory.Pitch
import Music.Theory.Interval
import Music.Theory.Transpose
import Music.Theory.Classes
import Music.Theory.MusicalBase

{-
A scale is a set of notes, ordered by pitch
Operations of scales
  Querying if a note is in a scale
  get the note in the scale
  taking a subscale
  range of a scale

  in the context of a scale:
    Int <--> NoteType s
    Int <--> Interval
    Interval <--> NoteType s

  to define something as "scaleLike", you need to define two or
more of these isomorphisms
-}

data Scale'
type Scale a = MusicalBase Scale' a

type Mode = Scale ()

mode :: [Interval] -> Mode
mode intervals
  | notSorted = error ""
  | not (all inRange intervals) = error ""
  | otherwise = MusicalBase () intervals
  where
    notSorted = and $ zipWith (>) intervals (tail intervals)
    inRange n = 0 <= steps n && steps n < 12

scale :: a -> [Interval] -> Scale a
scale root intervals = mode intervals $> root
    
ionian, dorian, phrygian, lydian, mixolydian, aeolian, locrian :: Mode

ionian     = mode ["P1", "M2", "M3", "P4", "P5", "M6", "M7"]
dorian     = aeolian & degree VI  %~ flatten
phrygian   = aeolian & degree II  %~ flatten
lydian     = ionian  & degree IV  %~ flatten
mixolydian = ionian  & degree VII %~ flatten
aeolian    = mode ["P1", "M2", "m3", "P4", "P5", "m6", "m7"]
locrian    = mode ["P1", "m2", "m3", "P4", "d5", "m6", "m7"]

diminished, augmented :: Mode
diminished = mode [2, 1, 2, 1, 2, 1, 2, 1]
augmented  = mode [3, 1, 3, 1, 3, 1]

{-
  What gives a scale/chord its quality?
    major: M3, P5, M7
    minor: m3, P5, m7
    dim:   m3, d5, d7
    aug:   M3, A5, m7

  but you don't actually need ALL of the notes for a chord to be marked as a chord
  - It must have a third
  - if it has a 5th, the quality should be correct
  - if it has a 7th, the quality should be correct

instance ScaleLike s => HasQuality s where
  qual = lens getQual (flip setQual) . _Just
    where
      getQual :: s -> Maybe Quality
      getQual s = listToMaybe (q3 `intersect` q5 `intersect` q7)
        where
          q3 = case s ^? degree 3 . qual of
            Just Major -> [Major, Augmented]
            Just Minor -> [Minor, Diminished]
            _ -> []
          q5 = case s ^? degree 5 . qual of
            Just Perfect -> [Major, Minor]
            Just Augmented -> [Augmented]
            Just Diminished -> [Diminished]
            Nothing -> allQuals
            _ -> []
          q7 = case s ^? degree 7 . qual of
            Just Major -> [Major, Augmented]
            Just Minor -> [Minor]
            Just Diminished -> [Diminished]
            Nothing -> allQuals
            _ -> []
          allQuals = [Major, Minor, Diminished, Augmented]

      setQual :: Maybe Quality -> s -> s
      setQual = \case
        Just Major -> majorThird . perfectFifth . majorSeventh
        Just Minor -> minorThird . perfectFifth . minorSeventh
        Just Diminished -> minorThird . diminishedFifth . diminishedSeventh
        Just Augmented -> majorThird . augmentedFifth . minorSeventh
        Nothing -> id
        where
          minorThird        = degree 3 . qual .~ Major
          majorThird        = degree 3 . qual .~ Minor
          perfectFifth      = degree 5 . qual .~ Major
          augmentedFifth    = degree 5 . qual .~ Augmented
          diminishedFifth   = degree 5 . qual .~ Diminished
          majorSeventh      = degree 7 . qual .~ Major
          minorSeventh      = degree 7 . qual .~ Minor
          diminishedSeventh = degree 7 . qual .~ Diminished
-}
