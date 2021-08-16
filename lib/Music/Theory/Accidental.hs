{-
  Accidentals are very closely related to transposition
  We track the accidental of notes/chords/scales for easier comprehension
  After transposition, it is sometimes not possible to

  If we didn't need to track the accidental


-}

module Music.Theory.Accidental (
  Accidental (..), offset,
  HasAccidental, accidental,
  pattern DoubleFlat, pattern Flat, pattern Natural, pattern Sharp, pattern DoubleSharp,
  flat, sharp,
) where

import Music.Theory.Semitones

import Control.Lens

newtype Accidental = Offset Int
  deriving (Eq, Ord, Num)
    via Int

offset :: Int -> Accidental
offset n = case mod12 n of
  0  -> Natural
  1  -> Sharp
  2  -> DoubleSharp
  10 -> DoubleFlat
  11 -> Flat
  a  -> Offset a

pattern DoubleFlat  = Offset (-2) :: Accidental
pattern Flat        = Offset (-1) :: Accidental
pattern Natural     = Offset (0)  :: Accidental
pattern Sharp       = Offset (1)  :: Accidental
pattern DoubleSharp = Offset (2)  :: Accidental

instance Show Accidental where
  show = \case
    DoubleFlat  -> "bb"
    Flat        -> "b"
    Natural     -> ""
    Sharp       -> "#"
    DoubleSharp ->  "##"
    Offset n    -> "offset" <> show n

class HasAccidental s where
  accidental :: Lens' s Accidental

instance Semitones Accidental where
  steps (Offset n) = n

flat :: (HasAccidental t) => t -> t
flat = accidental %~ (+ Flat)

sharp :: (HasAccidental t) => t -> t
sharp = accidental %~ (+ Sharp)

-- TODO:
--class Enharmonic s where
--  spell :: Accidental -> s -> s
