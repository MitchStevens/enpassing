{-# LANGUAGE DerivingVia, PatternSynonyms #-}
module Music.Theory.Degree where

import Control.Lens
import Data.Foldable
import Data.Function
import Data.Ord
import Prelude

import Music.Theory.Accidental
import Music.Theory.Transpose 

{-
  A scale degree is like an index of a musical object. Scales have degrees, the 1st degree is the first note, the second is the second note, etc. Chords also have degrees, but chord degrees are different still. To keep implementation as simple as possible, degrees are just with an accident.
-}

newtype Degree = Degree Int
  deriving (Eq, Ord) via Int

mkDegree :: Int -> Degree
mkDegree = Degree . (1-)

unDegree :: Degree -> Int
unDegree (Degree n) = n+1

pattern I    = Degree 0
pattern II   = Degree 1
pattern III  = Degree 2
pattern IV   = Degree 3
pattern V    = Degree 4
pattern VI   = Degree 5
pattern VII  = Degree 6
pattern VIII = Degree 7
pattern IX   = Degree 8
pattern X    = Degree 9
pattern XI   = Degree 10
pattern XII  = Degree 11
pattern XIII = Degree 12


instance Show Degree where
  show (Degree n) = toRoman (n+1)

instance Num Degree where
  (+) = undefined
  (-) = undefined
  (*) = undefined
  negate        = undefined
  abs = undefined
  signum   = undefined
  fromInteger   = mkDegree . fromIntegral

instance Enum Degree where
  toEnum = mkDegree
  fromEnum = unDegree

instance Semitones Degree where
  steps (Degree n) = 12*octaves + semitones
    where
      octaves = n `div` 7
      semitones = case n `mod` 7 of
        0 -> 0
        1 -> 2
        2 -> 4
        3 -> 5
        4 -> 7
        5 -> 9
        6 -> 11

  
toRoman :: Int -> String
toRoman x
  | x == 0 = ""
  | x < 0 = "Degree " <> show x
  | x >= 1000 = "M" <>  toRoman (x - 1000)
  | x >= 100 = digit "C" "D" "M" q' <> toRoman r'
  | x >= 10 = digit "X" "L" "C" q <> toRoman r
  | otherwise = digit "I" "V" "X" x
    where
      (q, r)   = x `divMod` 10
      (q', r') = x `divMod` 100

      digit :: String -> String -> String -> Int -> String
      digit a b c n = case n of
        1 -> a
        2 -> a<>a
        3 -> a<>a<>a
        4 -> a<>b
        5 -> b
        6 -> b<>a
        7 -> b<>a<>a
        8 -> b<>a<>a<>a
        9 -> a<>c
        _ -> "fail"

{-
  A infinite list of all the degrees starting from
  allDegrees = [i, ii, iii, iv, ...
-}
allDegrees :: [Degree]
allDegrees = mkDegree <$> [1..]

{-
alpha :: Degree -> List Interval
alpha (Degree n) = case n of
  0 -> [ natural i ]
  1 -> [ natural ii, flat ii, ]
  2 -> []
  3 -> []
  4 -> []
  5 -> []
  6 -> []
  _ -> []
  -}

isPerfect :: Degree -> Bool
isPerfect = (`elem` [0, 3, 4]) . (`mod` 7) . unDegree
