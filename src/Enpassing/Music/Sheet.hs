{-# LANGUAGE FlexibleInstances, LambdaCase #-}

module Enpassing.Music.Sheet (
  Bar,
  Sheet (..),
  bar_duration,
  as_bars
) where

import           Control.Monad
import           Data.Bifunctor
import           Data.List
import           Data.Monoid
import           Data.Ord
import           Data.Ratio
import           Data.Ratio
import qualified Data.Text                 as T
import           Enpassing.Music.Chord
import           Enpassing.Music.Key
import           Euterpea.Music            hiding (chord)
import           Test.QuickCheck.Gen

type Bar = [Primitive Chord]
data Sheet = Sheet { name      :: String,
                     sheet_key :: Key,
                     bars      :: [Primitive Chord] } deriving (Eq)

instance Show Sheet where
  show (Sheet name key bars) = name ++"\nKey: "++ show key ++
    "\n| "++ show_all_bars ++" |"
    where
      show_all_bars = intercalate " |\n| "
        . map (show_bar_line max_bar_length)  --pad the bars with
        . split4 $ all_bars                   --split into lists of bars with length 4

      max_bar_length = maximum $ map length all_bars :: Int
      all_bars = map (show_bar 0) (as_bars bars) :: [String]
      min_duration = (1 %) . minimum $ fmap (denominator.duration) bars :: Rational

      -- Split a list into sublists of length 4. Ensure that id == concat . split4.
      split4 :: [a] -> [[a]]
      split4 l = if length l <= 4
                  then [l]
                  else y : split4 z
        where (y, z) = splitAt 4 l

      show_bar_line :: Int -> [String] -> String
      show_bar_line n = intercalate " | " . map (pad_to n)

      show_bar :: Int -> Bar -> String
      show_bar x = unwords . map show_prim
        where
          n :: Rational -> Int
          n = fromInteger . numerator . (/ min_duration)

          show_prim :: Show a => Primitive a -> String
          show_prim p = pad_to x . unwords $ case p of
            Note d c -> show c : replicate (n d - 1) "/"
            Rest d   -> replicate (n d) "x"

pad_to :: Int -> String -> String
pad_to n str = str ++ replicate (n - length str) ' '

as_bars :: [Primitive Chord] -> [Bar]
as_bars chords = if null chords then [[]] else bar : as_bars rest
  where (bar, rest) = extract_bar 0 chords

extract_bar :: Dur -> [Primitive Chord] -> (Bar, [Primitive Chord])
extract_bar acc chords = case compare acc 1 of
  LT ->
    if null chords then ([], [Rest (1-acc)])
    else tup >>= extract_bar (acc + duration (head c))
      where tup@(c, _) = splitAt 1 chords
  EQ -> ([], chords)
  GT -> error "Chords didn't sum to one full bar"

bar_duration :: Bar -> Integer
bar_duration = round . sum . map duration

duration :: Primitive a -> Rational
duration = \case
  Note d _ -> d
  Rest d   -> d
