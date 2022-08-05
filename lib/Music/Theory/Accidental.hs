{-
  Accidentals are very closely related to transposition
  We track the accidental of notes/chords/scales for easier comprehension
  After transposition, it is sometimes not possible to

  If we didn't need to track the accidental

-}

module Music.Theory.Accidental where

import Control.Lens
import GHC.Generics
import Music.Theory.Semitones

newtype Accidental = Offset Int
  deriving (Generic)
  deriving
    (Eq, Ord, Num)
    via Int

offset :: Int -> Accidental
offset n =
  if mod12 n <= 6
    then Offset (mod12 n)
    else Offset (mod12 n - 12)

pattern DoubleFlat = Offset (-2) :: Accidental
pattern Flat = Offset (-1) :: Accidental
pattern Natural = Offset (0) :: Accidental
pattern Sharp = Offset (1) :: Accidental
pattern DoubleSharp = Offset (2) :: Accidental

-- should the double flat and double sharp unicode symbols be used?
instance Show Accidental where
  show (Offset n)
    | n > 0 = replicate n '#'
    | otherwise = replicate (-n) 'b'

instance Semitones Accidental where
  steps (Offset n) = n

instance Semigroup Accidental where
  (<>) = (+)

instance Monoid Accidental where
  mempty = Natural

-- Accidenal classes
class HasAccidental s where
  accidental :: Lens' s Accidental

doubleFlat, flat, sharp, doubleSharp :: HasAccidental a => a -> a
doubleFlat = flat . flat
flat = accidental %~ (+ Flat)
sharp = accidental %~ (+ Sharp)
doubleSharp = sharp . sharp

isFlat, isSharp :: HasAccidental a => a -> Bool
isFlat = (< 0) . view accidental
isSharp = (> 0) . view accidental

{-
  by definition of enharmonic spelling

    (not.isSharp) (flatKey a) == True
    (not.isFlat) (sharpKey a) == True

  all of these operations are idempotent
    flatKey (flatKey a) == flatKey a
    sharpKey (sharpKey a) == sharpKey a

  forall s. equiv s (flatKey s) == True
  forall s. equiv s (sharpKey s) == True

  equiv is reflexive, symetric, transitive

-}
class (Semitones a, HasAccidental a) => Enharmonic a where
  flatKey :: a -> a
  sharpKey :: a -> a
