module Music.Theory.Degree where

import Control.Lens
import Data.Foldable
import Data.Function
import Data.Ord
import GHC.Generics
import Music.Theory.Accidental
import Music.Theory.Semitones
import Prelude

newtype Degree = Degree Int deriving (Eq, Ord, Generic)

mkDegree :: Int -> Degree
mkDegree n = Degree (n - 1)

unDegree :: Degree -> Int
unDegree (Degree n) = n + 1

pattern I = Degree 0
pattern II = Degree 1
pattern III = Degree 2
pattern IV = Degree 3
pattern V = Degree 4
pattern VI = Degree 5
pattern VII = Degree 6
pattern VIII = Degree 7
pattern IX = Degree 8
pattern X = Degree 9
pattern XI = Degree 10
pattern XII = Degree 11
pattern XIII = Degree 12

instance Show Degree where
  show (Degree n) =
    if n >= 0
      then toRoman (n + 1)
      else "Degree (" <> show n <> ")"

instance Enum Degree where
  toEnum = mkDegree
  fromEnum = unDegree

instance Semitones Degree where
  steps (Degree n) = 12 * octaves + semitones
    where
      octaves = n `div` 7
      semitones = case Degree (n `mod` 7) of
        I -> 0
        II -> 2
        III -> 4
        IV -> 5
        V -> 7
        VI -> 9
        VII -> 11

instance Semigroup Degree where
  Degree a <> Degree b = Degree (a + b)

instance Monoid Degree where
  mempty = I

toRoman :: Int -> String
toRoman x
  | x > 1000 = "M" <> toRoman (x - 1000)
  | x == 1000 = "M"
  | x > 100 =
    let (q, r) = divMod x 100
     in digit "C" "D" "M" q <> toRoman r
  | x == 100 = "C"
  | x > 10 =
    let (q, r) = divMod x 10
     in digit "X" "L" "C" q <> toRoman r
  | x == 10 = "X"
  | x > 0 = digit "I" "V" "X" x
  | otherwise = error (show x <> " has no roman numeral")
  where
    digit :: String -> String -> String -> Int -> String
    digit a b c n = case n of
      0 -> ""
      1 -> a
      2 -> a <> a
      3 -> a <> a <> a
      4 -> a <> b
      5 -> b
      6 -> b <> a
      7 -> b <> a <> a
      8 -> b <> a <> a <> a
      9 -> a <> c
      _ -> "fail"

{-
  A infinite list of all the degrees starting from
  allDegrees = [i, ii, iii, iv, ...
-}
allDegrees :: [Degree]
allDegrees = mkDegree <$> [1 ..]

isPerfect :: Degree -> Bool
isPerfect (Degree n) = (n `mod` 7) `elem` [0, 3, 4]
