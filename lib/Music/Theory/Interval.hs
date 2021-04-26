{-# LANGUAGE RankNTypes, PatternSynonyms #-}
module Music.Theory.Interval (
  Interval (..),
  stepsToInterval,
  triad
) where

import Control.Lens
import Data.Function (on)
import Data.Char
import Data.String

import Music.Theory.Degree
import Music.Theory.Transpose
import  Music.Theory.Quality (ChordQuality)
import qualified Music.Theory.Quality as Q

data IntervalQuality = Major | Minor | Diminished | Augmented deriving Show
pattern Perfect = Major

data Interval = Interval
  { intervalQuality :: IntervalQuality
  , intervalDegree :: Degree }

instance Eq Interval where
  (==) = (==) `on` steps

instance Ord Interval where
  compare = compare `on` steps

instance Show Interval where
  show (Interval Major degree)
      | isPerfect degree = "Perfect " <> show degree
      | otherwise        = "Major "   <> show degree
  show (Interval quality degree) = 
    show quality <> " " <> show degree

instance Num Interval where
  a + b = stepsToInterval (steps a + steps b)
  a - b = stepsToInterval (steps a - steps b)
  a * b = stepsToInterval (steps a * steps b)
  abs = id
  signum = id
  fromInteger = stepsToInterval . fromInteger

instance IsString Interval where
  fromString (c1:c2:[]) =
    let
      d = mkDegree (digitToInt c2)
      q = case c1 of
        'M' -> Major
        'P' -> Perfect
        'm' -> Minor
        'd' -> Diminished
        'A' -> Augmented
        _ -> error (c1:" is not a valid interval quality")
    in
      if isDigit c2 then Interval q d else error ""

instance Enum Interval where
  toEnum = stepsToInterval
  fromEnum = steps

instance Real Interval where
  toRational = toRational . steps

instance Integral Interval where
  quotRem a b = (stepsToInterval a', stepsToInterval b')
    where (a', b') = quotRem (steps a) (steps b)
  toInteger = toInteger . steps

instance Semitones Interval where
  steps (Interval quality degree) = steps degree +
    case quality of
      Major      -> 0
      Minor      -> -1
      Diminished -> if isPerfect degree then (-1) else (-2)
      Augmented  -> 1

instance Transpose Interval where
  shift n = stepsToInterval . (fromIntegral n +) . steps

--instance ConstructQuality (Degree -> Interval) where
--  constructQuality = Interval
--
--instance {- OVERLAPPING -} HasQuality Interval where
--  qual = lens intervalQuality (\(Interval _ d) q -> Interval q d)

stepsToInterval :: Int -> Interval
stepsToInterval n = Interval quality degree'
  where
    deg = case mod12 n of
      0  -> "P1"
      1  -> "m2"
      2  -> "M2"
      3  -> "m3"
      4  -> "M3"
      5  -> "P4"
      6  -> "d5"
      7  -> "P5"
      8  -> "m6"
      9  -> "M6"
      10 -> "m7"
      11 -> "M7"
      _ -> error "impossible"
    Interval quality (Degree d) = deg
    octaves = div12 n
    degree' = Degree (7 * octaves + d) 

triad :: ChordQuality -> [Interval]
triad = \case
  Q.Major ->      ["P1", "m3", "P5"]
  Q.Minor ->      ["P1", "M3", "P5"]
  Q.Diminished -> ["P1", "m3", "d5"]
  Q.Augmented ->  ["P1", "M3", "A5"]
  Q.Sus2 ->       ["P1", "M2", "P5"]
  Q.Sus4 ->       ["P1", "P4", "P5"]
  

