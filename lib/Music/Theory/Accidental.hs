module Music.Theory.Accidental (
  Accidental (..),
  HasAccidental, accidental,
  pattern DoubleFlat, pattern Flat, pattern Natural, pattern Sharp, pattern DoubleSharp,
  flatten, sharpen,
) where

import Music.Theory.Semitones

import Control.Lens

newtype Accidental = Offset Int
  deriving (Eq, Ord, Num)
    via Int

pattern DoubleFlat  = Offset (-2) :: Accidental
pattern Flat        = Offset (-1) :: Accidental
pattern Natural     = Offset (0)  :: Accidental
pattern Sharp       = Offset (1)  :: Accidental
pattern DoubleSharp = Offset (2)  :: Accidental

instance Show Accidental where
  show = \case
    DoubleFlat  -> "𝄫"
    Flat        -> "♭"
    Natural     -> ""
    Sharp       -> "♯"
    DoubleSharp ->  "𝄪"
    Offset n    -> "offset" <> show n

class HasAccidental s where
  accidental :: Lens' s Accidental

instance Semitones Accidental where
  steps (Offset n) = n

flatten :: (HasAccidental t) => t -> t
flatten = accidental %~ (+ Flat)

sharpen :: (HasAccidental t) => t -> t
sharpen = accidental %~ (+ Sharp)


------
class Enharmonic s where
  spell :: Accidental -> s -> s
