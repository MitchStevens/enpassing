{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
module Enpassing.Theory.Pitch where

import           Enpassing.Theory.Accidental
import           Enpassing.Theory.Transpose

import           Control.Lens
import           Data.Function           (on)
import           Data.Ord                (comparing)
import           Debug.Trace

data Note = A | B | C | D | E | F | G
  deriving (Eq, Show, Enum, Bounded)
newtype PitchClass = PitchClass (Accidental Note)
  deriving (Show)
instance Eq PitchClass where
  (==) = (==) `on` semitones
instance Ord PitchClass where
  compare = compare `on` semitones
instance Semigroup PitchClass where
  (<>) p1 p2 = pitchClass (semitones p1 + semitones p2)
instance Monoid PitchClass where
  mempty = pitchClass 0
instance Transpose PitchClass where
  shift n = pitchClass . (n+) . semitones
instance (t ~ PitchClass) => Rewrapped PitchClass t
instance Wrapped PitchClass where
  type Unwrapped PitchClass = Accidental Note
  _Wrapped' = iso (\(PitchClass x) -> x) PitchClass

allPitchClasses :: [PitchClass]
allPitchClasses =  PitchClass <$>
  [ Natural C, Sharp C,   Natural D, Flat E
  , Natural E, Natural F, Sharp F,   Natural G
  , Flat A,    Natural A, Flat B,    Natural B ]

pitchClass :: Int -> PitchClass
pitchClass n = allPitchClasses !! mod12 n

flat, natural, sharp :: Note -> PitchClass
flat    = pitchClass . (\x -> x-1) . semitones
natural = pc
sharp   = pitchClass . (+) 1 . semitones

--A Pitch is a pitchclass with an octave. Modeling a pitch in scientific notation.
data Pitch = Pitch PitchClass Int deriving (Eq)
instance Ord Pitch where
  compare = comparing semitones
instance Show Pitch where
    show (Pitch pc oct) = show pc <> show oct
instance Transpose Pitch where
  shift n = pitch . (n+) . semitones
instance Semigroup Pitch where
  pitch1 <> pitch2 = pitch (semitones pitch1 + semitones pitch2)
instance Monoid Pitch where
  mempty = Pitch (natural C) 0

pitch :: Int -> Pitch
pitch n = Pitch (pitchClass n) (div n 12)

class Pitched a where
  pc :: a -> PitchClass
  semitones :: a -> Int

instance Pitched Note where
  pc = PitchClass . Natural
  semitones = \case
    { C -> 0; D -> 2; E -> 4; F -> 5
    ; G -> 7; A -> 9; B -> 11 }

instance Pitched PitchClass where
  pc = id
  semitones p = mod12 $ case p^._Wrapped of
    Flat    x -> semitones x - 1
    Natural x -> semitones x
    Sharp   x -> semitones x + 1

instance Pitched Pitch where
  pc (Pitch p _) = p
  semitones (Pitch pc octave) = semitones pc + octave * 12
