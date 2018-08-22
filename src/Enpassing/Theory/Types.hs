{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Enpassing.Theory.Types where

import           Control.Lens

import           Enpassing.Theory.Accidental
import           Enpassing.Theory.Pitch


--Degree
newtype Degree = Degree Int
  deriving (Eq, Ord, Show, Bounded)

data LetterCase a = Upper a | Lower a
  deriving (Eq, Show, Functor, Foldable, Traversable)


--Duration
type Duration = Rational


--Extension
data Extension = Acc (Accidental Degree) | Sus2 | Sus4 | No Degree
  deriving (Eq, Show)


--Key
type Key = (PitchClass, Mode)


--Mode
data Mode
  = Ionian
  | Dorian
  | Phrygian
  | Lydian
  | Mixolydian
  | Aeolian
  | Locrian
  | Augmented
  | Diminished
    deriving (Eq, Show, Ord, Enum, Bounded)


--Scale
data Scale = Scale { _rootS :: Pitch, _modeS :: Mode }
  deriving (Eq, Show)
makeLenses ''Scale

--Time Signature
data TimeSignature = TimeSignature Int Int
