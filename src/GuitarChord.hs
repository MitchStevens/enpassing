-- | 

module GuitarChord where

import Control.Lens
import Data.Maybe (catMaybes)

import Music.Theory
import Hand

newtype Fret = Fret { unfret :: Int }
  deriving (Eq, Ord)
type OpenString = ()

data GuitarChordF a = GuitarChord
  { sE :: Maybe a, sA :: Maybe a, sD :: Maybe a
  , sG :: Maybe a, sB :: Maybe a, se :: Maybe a }
  deriving (Eq)

type MoveableChord = GuitarChordF Fret
type OpenChord = GuitarChordF (Either OpenString Fret)

instance Functor GuitarChordF where
  fmap f = fromList . (fmap.fmap) f . toList
    
instance Foldable GuitarChordF where
  foldMap f = foldMap f . catMaybes . toList

instance Traversable GuitarChordF where
  traverse f = fmap fromList . (traverse.traverse) f . toList

instance Semitones Fret where
   steps = unfret

instance Transpose Fret where
  shift n (Fret x) = Fret (x + fromIntegral n)

instance Transpose MoveableChord where
  shift n chord = fmap (shift offset) chord
    where
      smallestFret = minimum chord
      offset = mod12 . steps $ shift n smallestFret `above` (Fret 0)
      

fromList :: [Maybe a] -> GuitarChordF a
fromList [sE, sA, sD, sG, sB, se] =
  GuitarChord sE sA sD sG sB se

toList :: GuitarChordF a -> [Maybe a]
toList (GuitarChord sE sA sD sG sB se) =
  [sE, sA, sD, sG, sB, se]

toOpenChord :: MoveableChord -> OpenChord
toOpenChord = fmap Right


--------------------------------------------------
data GuitarPosition = Position { str :: Int, fret :: Fret }
type GuitarFingering = HandF GuitarPosition


