{-# LANGUAGE LambdaCase #-}
module Enpassing.Theory.Mode (
  chordModes,
  getIntervals,
  numNotes
) where

import           Enpassing.Theory.Degree
import           Enpassing.Theory.Types

import           Control.Lens

chordModes :: [Mode]
chordModes = [Ionian, Aeolian, Mixolydian, Augmented, Diminished]

getIntervals :: Mode -> [Int]
getIntervals = \case
  Ionian     -> scaleMode d1
  Dorian     -> scaleMode d2
  Phrygian   -> scaleMode d3
  Lydian     -> scaleMode d4
  Mixolydian -> scaleMode d5
  Aeolian    -> scaleMode d6
  Locrian    -> scaleMode d7
  Diminished -> [2, 1, 2, 1, 2, 1, 2, 1]
  Augmented  -> [3, 1, 3, 1, 3, 1]

{- Get a mode of the major scale, 1 is major, 2 is Dorian, 3 is Phyrigian, etc. starting from a root pitch -}
scaleMode :: Degree -> [Int]
scaleMode deg =
  let major_intervals = [2, 2, 1, 2, 2, 2, 1]
      rotate n list = drop (mod n 7) list <> take (mod n 7) list
  in  rotate (review mkDegree deg) major_intervals

numNotes :: Mode -> Int
numNotes mode = case mode of
  Diminished -> 8
  Augmented  -> 6
  _          -> 7
