{-# LANGUAGE FlexibleInstances #-}

module Enpassing.Music.Sheet (
  Bar,
  Key,
  Sheet (..),
  bar_duration,
  as_bars
) where

import Enpassing.Music.Chord
import Enpassing.Music.Key
import Euterpea.Music hiding (chord)
import Test.QuickCheck.Gen
import Control.Monad
import Control.Monad.Identity
import Control.Monad.Writer.Lazy
import Data.Bifunctor
import Data.List
import Data.Monoid
import Data.Ratio
import Data.Ord
import qualified Data.Text as T

type Bar = [Primitive Chord]
data Sheet = Sheet { name :: String,
                     sheet_key :: Key,
                     bars :: [Bar] } deriving (Eq)

instance Show Sheet where
  show (Sheet name key bars) = name ++"\nKey: "++ show key ++
    "\n| "++ show_all_bars ++" |"
    where
      show_all_bars = intercalate " |\n| "
        . map (show_bar_line max_bar_length)  --pad the bars with
        . split4 $ all_bars                   --split into lists of bars with length 4
        where
          max_bar_length = maximum $ map length all_bars :: Int
          all_bars = map (show_bar 0) bars

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
          min_duration = 1 % 2
          n :: Rational -> Int
          n = fromInteger . numerator . (/ min_duration)

          show_prim :: Show a => Primitive a -> String
          show_prim p = pad_to x . unwords $ case p of
            Note d c -> show c : replicate (n d - 1) "/"
            Rest d   -> replicate (n d) "x"

pad_to :: Int -> String -> String
pad_to n str = str ++ replicate (n - length str) ' '

as_bars :: [Primitive Chord] -> [Bar]
as_bars chords =
  if null chords
    then pure []
    else let (crds, bar) = runWriter $ extract_bar 0 chords
         in bar : as_bars crds

extract_bar :: Dur -> [Primitive Chord] -> Writer Bar [Primitive Chord]
extract_bar acc chords = case compare acc 1 of
  LT -> if not $ null chords
    then (WriterT . Identity $ (ta, [he])) >>= extract_bar (acc + duration he)
    else pure [Rest (1-acc)]
      where (he, ta) = (head chords, tail chords)
  EQ -> pure chords
  GT -> error "Chords didn't sum to one full bar"

bar_duration :: Bar -> Integer
bar_duration = round . sum . map duration

duration :: Primitive a -> Rational
duration (Note d _) = d
duration (Rest d)   = d
