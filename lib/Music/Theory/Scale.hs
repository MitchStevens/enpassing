{-# LANGUAGE RankNTypes, TemplateHaskell, IncoherentInstances  #-}
module Music.Theory.Scale where

import Control.Lens
import Data.List (intersect)
import Data.Maybe (listToMaybe)
import Control.Monad.Fail
import Control.Monad.Reader

import Music.Theory.Accidental
import Music.Theory.Degree
import Music.Theory.Quality
import Music.Theory.Pitch
import Music.Theory.Interval
import Music.Theory.Transpose
import Music.Theory.Classes

{-
A scale is a set of notes, ordered by pitch
Operations of scales
  Querying if a note is in a scale
  get the note in the scale
  taking a subscale
  range of a scale

  in the context of a scale:
    Int <--> NoteType s
    Int <--> Interval
    Interval <--> NoteType s

  to define something as "scaleLike", you need to define two or
more of these isomorphisms
-}

class ScaleLike s where
  type NoteType s :: *
  findDegree    :: (MonadFail m, MonadReader s m) => Int -> m Degree
  findNote      :: (MonadFail m, MonadReader s m) => Int -> m (NoteType s)
  indexOfDegree :: (MonadFail m, MonadReader s m) => Degree -> m Int
  indexOfNote   :: (MonadFail m, MonadReader s m) => NoteType s -> m Int

  indexAllOf :: ScaleLike s => t Int -> Traversal' s Interval
  indexAllOf

  degree :: Degree -> Traversal' s Interval

arpeggiate :: ScaleLike s => s -> [NoteType s]
arpeggiate s = catMaybes $ bassNote : takeWhile isJust . map (runReaderT findNote s) $ [0..]
  where
    bassNote = runReaderT findNote s (-1)

newtype Mode = Mode { _modeIntervals :: [Interval] }
makeLenses ''Mode

data Scale = Scale { _scaleRoot :: PitchClass, _scaleMode :: Mode }
makeLenses ''Scale

--data TrueScale = TrueScale

instance ScaleLike Mode where
  type NoteType Mode = Interval
  degree d = modeIntervals . traverse . filtered ((d==) . intervalDegree)
  arpeggiate = _modeIntervals

instance ScaleLike Scale where
  type NoteType Scale = PitchClass
  degree d = scaleMode . degree d
  arpeggiate (Scale r mode) =
    (`shift` r) <$> arpeggiate mode

instance Semitones Scale where
  steps = steps @PitchClass . view root

instance Transpose Scale where
  shift n = root %~ (shift @PitchClass n)

instance HasRoot Scale PitchClass where
  root = scaleRoot

newMode :: [Interval] -> Mode
newMode intervals
  | notSorted = error ""
  | not (all inRange intervals) = error ""
  | otherwise = Mode intervals
  where
    notSorted = and $ zipWith (>) intervals (tail intervals)
    inRange n = 0 <= steps n && steps n < 12
   
newScale :: PitchClass -> Mode -> Scale
newScale = Scale 

interval :: ScaleLike s => Interval -> Traversal' s Interval
interval (Interval q d) = degree d . filtered ((q==) . intervalQuality)

containsNote :: ScaleLike s => NoteType s -> Bool
containsNote

--findDegree :: Scale -> Prism' Pitch Degree
--findDegree scale = prism' destruct construct
--  where
--    destruct :: Degree -> Pitch
--    destruct deg =
--      let
--        (octave, notes) = divMod (deg^.re mkDegree - 1) (scale^.mode.to numNotes)-- n >= 0
--        halfSteps = sum . take notes $ getIntervals (scale^.mode)
--      in shift (12*octave + halfSteps) (scale^.root)
--
--    construct :: Pitch -> Maybe Degree
--    construct (Pitch p n) = do
--      index <- elemIndex p (toPitchClasses scale)
--      index ^? mkDegree

numNotes :: ScaleLike s => s -> Int
numNotes = length . arpeggiate

ionian, dorian, phrygian, lydian, mixolydian, aeolian, locrian :: Mode

ionian     = Mode ["P1", "M2", "M3", "P4", "P5", "M6", "M7"]
dorian     = aeolian & degree VI  %~ flatten
phrygian   = aeolian & degree II  %~ flatten
lydian     = ionian  & degree IV  %~ flatten
mixolydian = ionian  & degree VII %~ flatten
aeolian    = Mode ["P1", "M2", "m3", "P4", "P5", "m6", "m7"]
locrian    = Mode ["P1", "m2", "m3", "P4", "d5", "m6", "m7"]

diminished, augmented :: Mode
diminished = Mode [2, 1, 2, 1, 2, 1, 2, 1]
augmented  = Mode [3, 1, 3, 1, 3, 1]

{-
  What gives a scale/chord its quality?
    major: M3, P5, M7
    minor: m3, P5, m7
    dim:   m3, d5, d7
    aug:   M3, A5, m7

  but you don't actually need ALL of the notes for a chord to be marked as a chord
  - It must have a third
  - if it has a 5th, the quality should be correct
  - if it has a 7th, the quality should be correct
-}
instance ScaleLike s => HasQuality s where
  qual = lens getQual (flip setQual) . _Just
    where
      getQual :: s -> Maybe Quality
      getQual s = listToMaybe (q3 `intersect` q5 `intersect` q7)
        where
          q3 = case s ^? degree 3 . qual of
            Just Major -> [Major, Augmented]
            Just Minor -> [Minor, Diminished]
            _ -> []
          q5 = case s ^? degree 5 . qual of
            Just Perfect -> [Major, Minor]
            Just Augmented -> [Augmented]
            Just Diminished -> [Diminished]
            Nothing -> allQuals
            _ -> []
          q7 = case s ^? degree 7 . qual of
            Just Major -> [Major, Augmented]
            Just Minor -> [Minor]
            Just Diminished -> [Diminished]
            Nothing -> allQuals
            _ -> []
          allQuals = [Major, Minor, Diminished, Augmented]

      setQual :: Maybe Quality -> s -> s
      setQual = \case
        Just Major -> majorThird . perfectFifth . majorSeventh
        Just Minor -> minorThird . perfectFifth . minorSeventh
        Just Diminished -> minorThird . diminishedFifth . diminishedSeventh
        Just Augmented -> majorThird . augmentedFifth . minorSeventh
        Nothing -> id
        where
          minorThird        = degree 3 . qual .~ Major
          majorThird        = degree 3 . qual .~ Minor
          perfectFifth      = degree 5 . qual .~ Major
          augmentedFifth    = degree 5 . qual .~ Augmented
          diminishedFifth   = degree 5 . qual .~ Diminished
          majorSeventh      = degree 7 . qual .~ Major
          minorSeventh      = degree 7 . qual .~ Minor
          diminishedSeventh = degree 7 . qual .~ Diminished
