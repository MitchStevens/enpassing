{-# LANGUAGE PostfixOperators #-}
module Music.Theory.Pitch where

import Control.Lens hiding ((#))
import Data.Function           (on)
import Data.List (nub, sort)
import Data.Ord                (comparing)
import Data.Foldable
import Debug.Trace

import Music.Theory.Accidental
import Music.Theory.Degree
import Music.Theory.Interval
import Music.Theory.Semitones
import Music.Theory.Note
import Music.Theory.Transpose

data NoteBase a = NoteBase
  { _noteName :: NoteName
  , _noteAccidental :: Accidental
  , _noteOctave :: a }
makeLenses ''NoteBase

-- notes without octave
type PitchClass = NoteBase ()
-- notes with octave
type Pitch      = NoteBase Int

-- PitchClass: A Note with an accidental
instance Semitones a => Eq (NoteBase a) where
  (==) = (==) `on` steps

instance Show PitchClass where
  show (NoteBase noteName noteAccidental ()) =
    show noteName <> show noteAccidental
instance Show Pitch where
  show (NoteBase noteName noteAccidental noteOctave) =
    show noteName <> show noteAccidental <> show noteOctave

instance Semitones a => Ord (NoteBase a) where
  compare = compare `on` steps

instance Functor NoteBase where
  fmap = over noteOctave

instance Semitones a => Semitones (NoteBase a) where
  steps nb = sum
    [nb^.noteName.to steps, nb^.accidental.to steps, nb^.noteOctave.to steps * 12]

{-
  How to shift by an interval:
    1. shift notename up or down based on degree (note)
    2. add accidentals to make
    3. deal with the octave

    name2 + acc2 = name1 + acc1 + interval
    acc2 = offset (name1 + acc1 + interval - name2)
    -- because offset takes accidental mod 12
    acc2 = offset (steps note + steps interval - name2)

    note2 + acc2 + oct2*12 = note1 + acc1 + oct1*12 + interval
    oct2*12 = note1 + acc1 + oct1*12 + interval - note2 - acc2
    oct2 = oct1 + div (note1 + acc1 + interval - note2 - acc2)

-}
instance (Enum a, Bounded a, Semitones a) => Transpose (NoteBase a) where
  shift interval@(Interval _ (Degree n)) note@(NoteBase oldName oldAcc oldOctave) =
    note
      & noteName .~ newName
      & noteAccidental .~ newAcc
      & noteOctave .~ newOctave
    where
      note1 = steps oldName
      note2 = steps newName
      acc1 = steps oldAcc
      acc2 = steps newAcc

      newName = shiftWithOverflow n oldName
      newAcc = offset (steps interval + note1 + acc1 - note2)
      newOctave = shiftWithOverflow (div12 (steps interval + note1 + acc1 - note2 - acc2)) oldOctave

shiftWithOverflow :: forall a. (Enum a, Bounded a) => Int -> a -> a
--shiftWithOverflow n a = trace ("&&& -> " <> show (fromEnum a + n) <> show cardinality) $ toEnum ((fromEnum a + n) `mod` cardinality)
shiftWithOverflow n a = toEnum ((fromEnum a + n) `mod` cardinality)
  where
    cardinality = 1 + fromEnum (maxBound @a) - fromEnum (minBound @a)

instance HasAccidental (NoteBase a) where
  accidental = noteAccidental

newPitchClass :: NoteName -> Accidental -> PitchClass
newPitchClass note acc = NoteBase note acc ()

c, d, e, f, g, a, b :: PitchClass
[c, d, e, f, g, a, b] = [ newPitchClass p Natural | p <- [C .. B] ]
  
allPitchClasses :: [PitchClass]
allPitchClasses = do
  n <- [c, d, e, f, g, a, b]
  [flat n, n, sharp n]

(%) :: NoteBase a -> Int -> Pitch
(%) p x = set noteOctave x p
