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
import Music.Theory.Scale
import Music.Theory.Semitones
import Music.Theory.Transpose


data Chord'
type Chord a = MusicalBase Chord' a

{-
  Chord ()          = chords: minor, dominant 7th, major 9th
  Chord PitchClass  = chords: Em, G9, Cm7(b5)
  Chord Pitch       = chords: C5+E5+G5+B5+D6
-}

data ChordQuality
  = Diminished
  | Minor
  | Major
  | Augmented
  | Sus2
  | Sus4

instance Show ChordQuality where
  show = \case
    Minor      -> "m"
    Major      -> "maj"
    Augmented  -> "aug"
    Diminished -> "dim"
    Sus2       -> "sus2"
    Sus4       -> "sus4"
    _          -> error "Mode Parse Error"

-- TODO: create show instances for chords
instance Show a => Show (Chord a) where
  show (MusicalBase base offsets) = show base <> " " <> show offsets

newChord :: a -> [Interval] -> Chord a
newChord = MusicalBase

exts :: HasIntervals a => Traversal' a Interval
exts = intervals . traverse . filtered (> "P5")

--- Show Chords
--instance Show Chord where
--  show chord = fromMaybe chordDescription $ do
--    let rootNote = chord ^. root . to (show @PitchClass)
--    quality <- chord ^? qual . to show
--    let extensions = exts chord
--    pure $ printf "%s%s" rootNote quality
--      where chordDescription = "couldn't show chord!"

chord :: a -> ChordQuality -> [Interval] -> Chord a
chord root qual exts = newChord root (triad qual <> exts)

-- Variations on chords
(/) :: (HasRoot (chord a) a, HasIntervals (chord a), Semitones a) => a -> chord a -> chord a
(/) bass chord = over intervals (bassInterval:) chord
  where bassInterval = stepsToInterval $ steps (chord^.root) - steps bass

bass :: HasIntervals t => Traversal' t Interval
bass = intervals . traverse . filtered (< 0)

triad :: ChordQuality -> [Interval]
triad = \case
  Major      -> ["P1", "M3", "P5"]
  Minor      -> ["P1", "m3", "P5"]
  Diminished -> ["P1", "m3", "d5"]
  Augmented  -> ["P1", "M3", "A5"]
  Sus2       -> ["P1", "M2", "P5"]
  Sus4       -> ["P1", "P4", "P5"]

data Inversion chord a = Inversion
  { _inversionNum :: Int
  , _inversionChord :: chord a }
makeLenses ''Inversion
