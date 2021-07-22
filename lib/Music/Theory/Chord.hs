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
import Music.Theory.Semitones
import Music.Theory.Transpose


data Chord'
type Chord a = MusicalBase Chord' a

{-
  Chord ()          = chords: minor, dominant 7th, major 9th
  Chord PitchClass  = chords: Em, G9, Cm7(b5)
  Chord Pitch       = chords: C5+E5+G5+B5+D6
-}

-- TODO: create show instances for chords
instance Show (Chord a) where
  show _ = ""

newChord :: a -> [Interval] -> Chord a
newChord = MusicalBase

exts :: Chord a -> [Interval]
exts = view $ intervals . to (filter (> "P5"))

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

data Inversion chord a = Inversion
  { _inversionNum :: Int
  , _inversionChord :: chord a }
makeLenses ''Inversion

---
