-- | 

module PianoChord where

import Music.Theory
import Hand

data PianoFingeringF a = PianoFingering
  { leftHand  :: HandF a
  , rightHand :: HandF a
  }
type PianoFingering = PianoFingeringF Pitch

instance Functor PianoFingeringF where
  fmap f (PianoFingering leftHand rightHand) =
    PianoFingering (fmap f leftHand) (fmap f rightHand)

instance Foldable PianoFingeringF where
  foldMap f (PianoFingering leftHand rightHand) =
    foldMap f leftHand <> foldMap f rightHand

validFingering :: PianoFingering -> Bool
validFingering = all ((==) 1 . length) . group . sort . foldl' (flip (:)) []
