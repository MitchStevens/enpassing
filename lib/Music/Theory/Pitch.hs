{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PostfixOperators #-}
module Music.Theory.Pitch where

import Control.Lens hiding ((#))
import Data.Function           (on)
import Data.List (nub, sort)
import Data.Ord                (comparing)
import Data.Foldable

import Music.Theory.Accidental
import Music.Theory.Semitones
import Music.Theory.Transpose

data NoteName = C | D | E | F | G | A | B
  deriving (Eq, Show, Enum, Bounded)

instance Semitones NoteName where
  steps n =  case n of
    { C -> 0; D -> 2; E -> 4; F -> 5
    ; G -> 7; A -> 9; B -> 11 }
    
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

instance Transpose PitchClass where
  shift n p = pitchClass $ steps p + fromIntegral n
instance Transpose Pitch where
  shift n p = pitch $ steps p + fromIntegral n

instance HasAccidental (NoteBase a) where
  accidental = noteAccidental

mkPitchClass :: NoteName -> Accidental -> PitchClass
mkPitchClass name acc = NoteBase name acc ()

mkPitch :: NoteName -> Accidental -> Int -> Pitch
mkPitch = NoteBase

c, d, e, f, g, a, b :: PitchClass
[c, d, e, f, g, a, b] = (\p -> NoteBase p Natural ()) <$> [C, D, E, F, G, A, B]
  
allPitchClasses :: [PitchClass]
allPitchClasses = nub . sort $
  [c, d, e, f, g, a, b] >>= (\n -> [(flat n), n, (sharp n)])

pitchClass :: Int -> PitchClass
pitchClass n = allPitchClasses !! mod12 n

pitch :: Int -> Pitch
pitch n = set noteOctave (div12 n) (pitchClass n)

(%) :: NoteBase a -> Int -> Pitch
(%) p x = set noteOctave x p
