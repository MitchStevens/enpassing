{-# LANGUAGE RankNTypes, PatternSynonyms #-}
module Music.Theory.Interval (
  Interval (..), newInterval,
  stepsToInterval,
  triad
) where

import Control.Lens
import Control.Monad
import Data.Function (on)
import Data.Functor
import Data.Maybe
import Data.Char
import Data.String

import Music.Theory.Degree
import Music.Theory.Transpose
import Music.Theory.Quality
import Music.Theory.Classes

data Interval = Interval
  { intervalQuality :: Quality
  , intervalDegree :: Degree }

instance Eq Interval where
  (==) = (==) `on` steps

instance Ord Interval where
  compare = compare `on` steps

instance Show Interval where
  show (Interval Major degree)
      | isPerfect degree = "Perfect" <> show degree
      | otherwise        = "Major" <> show degree
  show (Interval quality degree) = 
    show quality <> " " <> show degree

instance Num Interval where
  a + b = stepsToInterval (steps a + steps b)
  a - b = stepsToInterval (steps a - steps b)
  a * b = stepsToInterval (steps a * steps b)
  abs = id
  signum = id
  fromInteger = stepsToInterval . fromInteger

{-

-}
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
    in
      if isDigit c2 then newInterval q d else error ""

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

instance ConstructQuality (Degree -> Interval) where
  constructQuality = Interval

instance {- OVERLAPPING -} HasQuality Interval where
  qual = lens intervalQuality (\(Interval _ d) q -> Interval q d)

newInterval :: Quality -> Degree -> Interval
newInterval = Interval

-- intervalIso :: r -> Iso Interval r
-- intervalIso root = iso (\i -> shift i root) (\r -> stepsToInterval (root `diff` r))

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
    Interval quality (Degree d) = deg
    octaves = div12 n
    degree' = Degree (7 * octaves + d) 

triad :: Quality -> [Interval]
triad = \case
  Major -> ["P1", "m3", "P5"]
  Minor -> ["P1", "M3", "P5"]
  Diminished -> ["P1", "m3", "d5"]
  Augmented -> ["P1", "M3", "A5"]
