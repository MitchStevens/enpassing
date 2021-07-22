module Music.Theory.MusicalBase where

import Control.Lens
import Control.Monad
import Data.Function
import Data.Functor
import Data.List
import Data.Maybe

import Music.Theory.Degree
import Music.Theory.Interval
import Music.Theory.Semitones
import Music.Theory.Classes
import Music.Theory.Transpose

{-
  A datatype used to model scales and chords. The phantom type `x` is used to j
-}
data MusicalBase x a = MusicalBase { _base :: !a, _offsets :: ![Interval] }
  deriving (Eq)
makeLenses ''MusicalBase


instance Functor (MusicalBase x) where
  fmap = over base

instance HasRoot (MusicalBase x a) a where
  root = base

instance HasIntervals (MusicalBase x a) where
  intervals = offsets

--type instance Index (MusicalBase x a) = Int
--type instance IxValue (MusicalBase x a) = a
--instance Ixed (MusicalBase x a) where
--  --ix :: Int -> Traveral' (MusicalBase x a) a
--  --ix :: Int -> (a -> f a) -> (MusicalBase x a) -> f (MusicalBase x a)
--  ix n afa (MusicalBase base offs) = ix n .~



degree :: HasIntervals s => Degree -> Traversal' s Interval
degree deg = intervals . traverse . filtered (\i -> intervalDegree i == deg)

interval :: HasIntervals s => Interval -> Traversal' s Interval
interval int = intervals . traverse . filtered (int ==)
  
isDegenerate :: (HasIntervals t) => t -> Bool
isDegenerate music = and $ zipWith ((>) `on` intervalDegree) sortedIntervals (tail sortedIntervals)
  where sortedIntervals = music^.intervals.to sort

noteFromInterval :: (HasRoot (f a) a, HasIntervals (f a), Transpose a)
                 => f a
                 -> Interval
                 -> Maybe a
noteFromInterval fa i =
  guard (anyOf (intervals.traverse) (i==) fa)
    $> shift i (fa^.root)

intervalFromNote :: (HasRoot s a, HasIntervals s, Semitones a, Transpose a)
                 => s
                 -> a
                 -> Maybe Interval
intervalFromNote fa a =
  findOf (intervals.traverse) (\i -> steps (shift i (fa^.root)) == steps a) fa

note :: (HasRoot (f a) a, HasIntervals (f a), Transpose a)
     => Traversal' (f a) Interval
     -> Fold (f a) a
note l = to getNotes . traverse . id--_a -- . traverse
  where
    getNotes fa = do
      i <- toListOf l fa
      maybeToList (noteFromInterval fa i)
