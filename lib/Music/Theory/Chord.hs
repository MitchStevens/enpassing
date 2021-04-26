module Music.Theory.Chord where

import Control.Lens hiding (below)
import Text.Printf
import Data.Maybe
import Data.Function

import Music.Theory.Accidental
import Music.Theory.Classes
import Music.Theory.Degree
import Music.Theory.Interval
import Music.Theory.MusicalBase
import Music.Theory.Pitch
import Music.Theory.Quality
import Music.Theory.Scale
import Music.Theory.Transpose

{-
Chord Symbol:
 ChordSymbol: VIm
 Chord: Am7
 PreciseChord: A5 C5 E6 G6
-}

type ChordType    = MusicalBase ()
type ChordSymbol  = MusicalBase Degree
type Chord        = MusicalBase PitchClass
type ChordPrecise = MusicalBase Pitch

-- TODO: create show instances for chords
instance Show ChordType where
  show _ = ""
instance Show ChordSymbol where
  show _ = ""
instance Show Chord where
  show _ = ""
instance Show ChordPrecise where
  show _ = ""


newSymbol :: Degree -> [Interval] -> ChordSymbol
newSymbol = ChordSymbol

newChord :: PitchClass -> [Interval] -> Chord
newChord = Chord

exts :: ChordLike chord => chord -> [Interval]
exts = filter ((>5). intervalDegree) . arpeggiate

--- Show Chords
instance Show Chord where
  show chord = fromMaybe chordDescription $ do
    let rootNote = chord ^. root . to (show @PitchClass)
    quality <- chord ^? qual . to show
    let extensions = exts chord
    pure $ printf "%s%s" rootNote quality
      where chordDescription = "couldn't show chord!;"


--- ScaleLike & ChordLike
class ScaleLike s => ChordLike s

chord :: a -> ChordQuality -> [Interval] -> MusicalBase a
chord root qual exts = MusicalBase root (triad qual <> exts)

-- Variations on chords
(/) :: (HasRoot (chord a) a, HasIntervals (chord a), Semitones a) => a -> chord a -> chord a
(/) bass chord = over intervals (bassInterval:) chord
  where bassInterval = stepsToInterval $ steps (chord^.root) - steps bass

bass :: HasIntervals t => Traversal' t Interval
bass = intervals . traverse . filtered (< 0)

data Inversion chord a = Inversion
  { _inversionNum :: Int
  , _inversionChord :: chord a }
makeLenses ''Inversion

---
