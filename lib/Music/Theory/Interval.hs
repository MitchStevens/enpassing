module Music.Theory.Interval (
  Interval (..), intervalDegree, intervalQuality,
  stepsToInterval
) where

import Control.Lens
import Data.Function (on)
import Data.Char
import Data.String

import Music.Theory.Degree
import Music.Theory.Semitones
import Music.Theory.Accidental

data IntervalQuality = Major | Minor | Diminished | Augmented deriving Show
pattern Perfect = Major

data Interval = Interval
  { _intervalQuality :: IntervalQuality
  , _intervalDegree :: Degree }
makeLenses ''Interval

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

-- this is a weird instance and may not obey laws, double check this
-- This instance exists so we can use `flatten` and `sharpen` functions
instance HasAccidental Interval where
  accidental = lens get set
    where
      get (Interval quality degree) = case quality of
        Major      -> Natural
        Minor      -> Flat
        Diminished -> if isPerfect degree then Flat else DoubleFlat
        Augmented  -> Sharp
      set interval@(Interval quality degree) acc
        = maybe interval (\q -> Interval q degree) newQuality
        where
          newQuality = case acc of
            DoubleFlat ->
              if isPerfect degree then Nothing else Just Diminished
            Flat ->
              if isPerfect degree then Just Diminished else Just Minor
            Natural ->
              Just Major
            Sharp ->
              if isPerfect degree then Just Augmented else Nothing
            _ -> Nothing



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

