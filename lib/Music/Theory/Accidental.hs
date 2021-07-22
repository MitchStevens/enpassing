{-
  Accidentals are very closely related to transposition
  We track the accidental of notes/chords/scales for easier comprehension
  After transposition, it is sometimes not possible to

  If we didn't need to track the accidental


-}

module Music.Theory.Accidental (
  Accidental (..),
  HasAccidental, accidental,
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

instance Semitones Accidental where
  steps (Offset n) = n


doubleFlat, flat, natural, sharp, doubleSharp :: HasAccidental s => s -> s
doubleFlat  = set accidental DoubleFlat
flat        = set accidental Flat
natural     = set accidental Natural
sharp       = set accidental Sharp
doubleSharp = set accidental DoubleSharp

flatten, sharpen :: HasAccidental s => s -> s
flatten = over accidental (Offset . (\n -> n-1). steps)
sharpen = over accidental (Offset . (\n -> n+1). steps)

_DoubleFlat, _Flat, _Natural, _Sharp, _DoubleSharp 
  :: HasAccidental s 
  => Traversal' s ()
_DoubleFlat  = accidental . only DoubleFlat
_Flat        = accidental . only Flat
_Natural     = accidental . only Natural
_Sharp       = accidental . only Sharp
_DoubleSharp = accidental . only DoubleSharp
