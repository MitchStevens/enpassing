{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Music.Theory.Pitch where

import           Control.Lens
import           Data.Function           (on)
import Data.List (nub, sort)
import           Data.Ord                (comparing)

import           Music.Theory.Accidental
import           Music.Theory.Transpose

data Note = C | D | E | F | G | A | B
  deriving (Eq, Show, Enum, Bounded)

instance Semitones Note where
  steps n =  case n of
    { C -> 0; D -> 2; E -> 4; F -> 5
    ; G -> 7; A -> 9; B -> 11 }

data PitchClass = PitchClass { _pcNote :: Note, _pcAccidental :: Accidental }
makeLenses ''PitchClass

instance Eq PitchClass where
  (==) = (==) `on` steps

instance Show PitchClass where
  show (PitchClass note acc) = show note <> show acc

instance Ord PitchClass where
  compare = compare `on` steps

instance Semitones PitchClass where
  steps (PitchClass note acc) = mod12 (steps acc + steps note)

instance Transpose PitchClass where
  shift n pc = pitchClass $ steps pc + fromIntegral n

instance HasAccidental PitchClass where
  accidental = pcAccidental

instance ConstructAccidental PitchClass Note where
  constructAcc acc note = PitchClass note acc


allPitchClasses :: [PitchClass]
allPitchClasses = nub . sort $
  PitchClass
    <$> [C, D, E, F, G, A, B]
    <*> [Flat, Natural, Sharp]

pitchClass :: Int -> PitchClass
pitchClass n = allPitchClasses !! mod12 n


--A Pitch is a pitchclass with an octave. Modeling a pitch in scientific notation.
data Pitch = Pitch PitchClass Int deriving (Eq)

mkPitch :: Int -> Pitch
mkPitch n = Pitch (pitchClass n) (div12 n)

instance Ord Pitch where
  compare = comparing steps

instance Show Pitch where
  show (Pitch pc oct) = show pc <> show oct

instance Semitones Pitch where
  steps (Pitch pc octave) = steps pc + octave * 12

instance Transpose Pitch where
  shift n p = mkPitch (steps p + fromIntegral n)

