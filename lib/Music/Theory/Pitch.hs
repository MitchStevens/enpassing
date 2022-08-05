module Music.Theory.Pitch where

import Control.Lens hiding ((#))
import Data.Foldable
import Data.Function (on)
import Data.Functor (($>))
import Data.List (nub, sort)
import Data.Maybe (fromMaybe)
import Data.Ord (comparing)
import Debug.Trace
import GHC.Generics hiding (to)
import Music.Theory.Accidental
import Music.Theory.Classes
import Music.Theory.Degree
import Music.Theory.Interval
import Music.Theory.Note
import Music.Theory.Semitones
import Music.Theory.Transpose

type PitchLike p o = (HasAccidental p, HasPitchName p, HasOctave p o, Semitones p)

pitchClass :: (PitchLike p o) => Lens' p PitchClass
pitchClass = lens getPC setPC
  where
    getPC p = PitchClass (p ^. pitchName) (p ^. accidental)
    setPC p (PitchClass name acc) = set pitchName name . set accidental acc $ p

-- notes without octave
data NoOctave = NoOctave deriving (Show, Generic)

data PitchClass = PitchClass
  { _pcName :: NoteName,
    _pcAccidental :: Accidental
  }
  deriving (Eq, Generic)

data Pitch = Pitch
  { _pPitchClass :: PitchClass,
    _pOctave :: Int
  }
  deriving (Eq, Generic)

makeLenses ''Pitch

makeLenses ''PitchClass

instance HasAccidental PitchClass where
  accidental = pcAccidental

instance HasPitchName PitchClass where
  pitchName = pcName

instance HasOctave PitchClass () where
  pitchOctave f p = f () $> p

instance HasAccidental Pitch where
  accidental = pPitchClass . accidental

instance HasPitchName Pitch where
  pitchName = pPitchClass . pitchName

instance HasOctave Pitch Int where
  pitchOctave = pOctave

-- NoOctave instances
--instance Enum PitchCalss where
--  toEnum _ = NoOctave
--  fromEnum _ = 0

--instance (Semitones p, PitchLike p o) => Eq p where
--  (==) = (==) `on` steps

instance Show PitchClass where
  show (PitchClass name acc) =
    show name <> show acc

instance Show Pitch where
  show (Pitch (PitchClass name acc) octave) =
    show name <> show acc <> show octave

--instance (Semitones p, PitchLike p o) => Ord p where
--  compare = compare `on` steps

instance Semitones PitchClass where
  steps p =
    sum
      [ p ^. pitchName . to (steps :: NoteName -> Int),
        p ^. accidental . to (steps :: Accidental -> Int)
      ]

instance Semitones Pitch where
  steps p =
    sum
      [ p ^. pitchName . to (steps :: NoteName -> Int),
        p ^. accidental . to (steps :: Accidental -> Int),
        p ^. pitchOctave . to fromEnum * 12
      ]

instance Transpose PitchClass where
  -- shift :: Interval -> p -> p
  shift interval@(Interval acc (Degree n)) oldPitch = newPitch
    where
      newPitch =
        oldPitch
          & pitchName .~ newName
          & accidental .~ newAcc

      oldName = oldPitch ^. pitchName
      oldAcc = oldPitch ^. accidental
      newName = toEnum ((fromEnum oldName + n) `mod` 7)
      newAcc = offset (steps oldPitch + steps interval - steps newName)

instance Transpose Pitch where
  shift interval@(Interval acc (Degree n)) oldPitch = newPitch
    where
      newPitch =
        oldPitch
          & pitchName .~ newName
          & pitchOctave .~ newOct
          & accidental .~ newAcc

      oldName = oldPitch ^. pitchName
      oldOct = oldPitch ^. pitchOctave
      oldAcc = oldPitch ^. accidental
      newName = toEnum ((fromEnum oldName + n) `mod` 7)
      newOct = toEnum (fromEnum oldOct + (fromEnum oldName - fromEnum newName + n) `div` 7)
      newAcc = offset (steps oldPitch + steps interval - steps newName - 12 * fromEnum newOct)

{-
  the following hold:
    newName = oldName + n `mod` 7
    newOct = oldOct + (newName - oldName) `div` 7
    newAcc = steps old - steps newName - 12 * steps newOct

  TODO: fix error message?
-}

instance Enharmonic PitchClass where
  flatKey = fromMaybe (error "hallofdsaf") . find (not . isSharp) . iterate (enharmonicPitchClassShift (-1))
  sharpKey = fromMaybe (error "hallofdsaf") . find (not . isFlat) . iterate (enharmonicPitchClassShift 1)

enharmonicPitchClassShift :: Int -> PitchClass -> PitchClass
enharmonicPitchClassShift n old = new
  where
    new =
      old
        & pitchName .~ newName
        & accidental .~ newAcc

    oldName = old ^. pitchName
    oldAcc = old ^. accidental

    newName = toEnum ((fromEnum oldName - n) `mod` 7)
    newAcc = offset (steps old - steps newName)

--instance (Show (NoteBase a), Enum a, Semitones a) => Enharmonic (NoteBase a) where
--  flatKey = fromMaybe (error "hallofdsaf") . find (not . isSharp) . iterate (enharmonicShift (-1))
--  sharpKey = fromMaybe (error "hallofdsaf") . find (not . isFlat) . iterate (enharmonicShift 1)

newPitchClass :: NoteName -> Accidental -> PitchClass
newPitchClass note acc = PitchClass note acc

c, d, e, f, g, a, b :: PitchClass
[c, d, e, f, g, a, b] = [newPitchClass p Natural | p <- [C .. B]]

allPitchClasses :: [PitchClass]
allPitchClasses = do
  n <- [c, d, e, f, g, a, b]
  [flat n, n, sharp n]

(%) :: PitchLike p o => p -> Int -> Pitch
(%) p = Pitch (PitchClass (p ^. pitchName) (p ^. accidental))
