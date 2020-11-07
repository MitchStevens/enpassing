module Music.Theory.Chord where

import Control.Lens hiding (below)
import Data.Function

import Music.Theory.Accidental
import Music.Theory.Degree
import Music.Theory.Interval
import Music.Theory.Scale
import Music.Theory.Pitch
import Music.Theory.Transpose
import Music.Theory.Quality
import Music.Theory.Classes

{-
Chord Symbol:
 ChordSymbol: VIm
 Chord: Am7
 PreciseChord: A5 C5 E6 G6

-}
data ChordSymbol = ChordSymbol
  { _csPitchClass :: Degree
  , _csIntervals :: [Interval] }
makeLenses ''ChordSymbol

data Chord = Chord
  { _cRoot :: PitchClass
  , _cIntervals :: [Interval] }
makeLenses ''Chord

data ChordPrecise = ChordPrecise
  { _precisePitches :: Pitch }
makeLenses ''ChordPrecise

-- Variations on chords
data Slash chord = Slash
  { _bassNote :: NoteType chord
  , _slashChord :: chord }
makeLenses ''Slash

data Inversion chord = Inversion
  { _inversionNum :: Int
  , _inversionChord :: chord }
makeLenses ''Inversion

newSymbol :: Degree -> [Interval] -> ChordSymbol
newSymbol = ChordSymbol

newChord :: PitchClass -> [Interval] -> Chord
newChord = Chord

---
class ScaleLike s => ChordLike s

instance ScaleLike ChordSymbol where
  type NoteType ChordSymbol = Interval
  degree d = csIntervals . traverse . filtered (d==.intervalDegree)
  arpeggiate = view csIntervals
instance ChordLike ChordSymbol

instance ScaleLike Chord where
  type NoteType Chord = PitchClass
  degree d = cIntervals . traverse . filtered (d==.intervalDegree)
  arpeggiate (Chord root intervals) = (`shift` root) <$> intervals
instance ChordLike Chord

instance ScaleLike chord => ScaleLike (Slash chord) where
  type NoteType (Slash chord) = NoteType chord
  interval i = slashChord . interval i
  arpeggiate (Slash bass chord) = bass : arpeggiate chord
instance ChordLike chord => ChordLike (Slash chord)

instance ScaleLike chord => ScaleLike (Inversion chord) where
  type NoteType (Inversion chord) = NoteType chord
  interval i = inversionChord . interval i
  arpeggiate (Inversion num chord) = rotate num (arpeggiate chord)
    where rotate n list = drop n list <> take n list
instance ChordLike chord => ChordLike (Inversion chord)

-- HasRoot instances
instance HasRoot Chord PitchClass where
  root = cRoot

instance HasRoot chord note => HasRoot (Slash chord) note where
  root = slashChord . root

instance HasRoot chord note => HasRoot (Inversion chord) note where
  root = inversionChord . root

-- Transpose Instances
instance Semitones Chord where
  steps = steps . _cRoot

instance Transpose Chord where
  shift n = root %~ (shift n :: PitchClass -> PitchClass)

instance Semitones chord => Semitones (Slash chord) where
  steps = steps . _slashChord

instance (Transpose (NoteType chord), Transpose chord) => Transpose (Slash chord) where
  shift n crd = crd & bassNote %~ (shift n :: NoteType chord -> NoteType chord)
          & slashChord %~ (shift n :: chord -> chord)

instance Semitones chord => Semitones (Inversion chord) where
  steps = steps . _inversionChord

instance Transpose chord => Transpose (Inversion chord) where
  shift n = inversionChord %~ (shift n :: chord -> chord)

--useful for initialising slash chords, eg. C / fMajor creates a slash chord
(/) :: NoteType chord -> chord -> Slash chord
(/) = Slash


