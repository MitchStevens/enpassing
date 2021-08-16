-- | 

module GuitarChord where

import Control.Lens
import Data.Maybe (catMaybes)

import Music.Theory

newtype Fret = Fret Int
type GuitarString = Int
data Finger = Finger { fret :: Fret, barre :: (GuitarString, GuitarString) }
data GuitarFingering = HandF Finger

instance Semitones Finger where
   steps = fret

instance Transpose Fret where
  shift n (Finger fret barre) = Finger (shift n fret) barre

{-
  ,_________.
5 |-1*******1
  |-|-|-|-2-|
  |-|-4-3-|-|
  |-|-|-|-|-|
-}

fretted :: Fret -> GuitarString -> Finger
fretted fret string = Finger fret (string, string)

barred :: Fret -> (GuitarString, GuitarString) -> Finger
barred = Finger

chordDiagram :: MovableChord -> String
chordDiagram fingering = unlines $ emptyDiagram
  emptyDiagram
  where
    Fret initial = minimum $ fret <$> fingering
    Fret terminal = maximum $ fret <$> fingering

    emptyDiagram = [ ",_________." ] <>
      replicate (terminal - initial + 1) "|-|-|-|-|-|"

    writeStringToDiagram :: [String] -> Finger -> [String]


g = Hand
  Nothing
  Just (fretted (Fret 3) 4)
  Just (fretted (Fret 4) 3)
  Just (fretted (Fret 4) 2)
  Just (fretted (Fret 6) 1)

-- main = putStrLn . displayGuitarChord $ g
