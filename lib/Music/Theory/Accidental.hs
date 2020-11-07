module Music.Theory.Accidental (
  Accidental (..),
  HasAccidental, accidental,
  ConstructAccidental, constructAcc,
  pattern DoubleFlat, pattern Flat, pattern Natural, pattern Sharp, pattern DoubleSharp, 
  doubleFlat, flat, natural, sharp, doubleSharp, 
  _DoubleFlat, _Flat, _Natural, _Sharp, _DoubleSharp
) where

import Music.Theory.Transpose

import Control.Lens

newtype Accidental = Offset Int
  deriving (Eq, Ord)

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

class ConstructAccidental s a | a -> s, s -> a where
  constructAcc :: Accidental -> a -> s

instance Semitones Accidental where
  steps (Offset n) = n

instance Semitones a => Semitones (Accidental, a) where
  steps (acc, x) = steps acc + steps x


doubleFlat, flat, natural, sharp, doubleSharp :: ConstructAccidental s a => a -> s
doubleFlat  = constructAcc DoubleFlat
flat        = constructAcc Flat
natural     = constructAcc Natural
sharp       = constructAcc Sharp
doubleSharp = constructAcc DoubleSharp

_DoubleFlat, _Flat, _Natural, _Sharp, _DoubleSharp 
  :: HasAccidental s 
  => Traversal' s ()
_DoubleFlat  = accidental . only DoubleFlat
_Flat        = accidental . only Flat
_Natural     = accidental . only Natural
_Sharp       = accidental . only Sharp
_DoubleSharp = accidental . only DoubleSharp
