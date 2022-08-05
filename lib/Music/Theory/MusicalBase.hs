{-
  A datatype used to model scales and chords. The phantom type `x` is used to j
-}
{-# LANGUAGE GADTs #-}

module Music.Theory.MusicalBase where

import Control.Lens
import Control.Monad
import Data.Function
import Data.Functor
import Data.List
import Data.Maybe
import Music.Theory.Classes
import Music.Theory.Degree
import Music.Theory.Interval
import Music.Theory.Semitones
import Music.Theory.Transpose

data MusicalBase x a where
  MusicalBase :: Transpose a => a -> [Interval] -> MusicalBase x a

makeLenses ''MusicalBase

instance HasRoot (MusicalBase x a) a where
  root f (MusicalBase base offsets) = fmap (\b' -> MusicalBase b' offsets) (f base)

instance HasIntervals (MusicalBase x a) where
  intervals f (MusicalBase base offsets) = fmap (\o' -> MusicalBase base o') (f offsets)

instance Functor (MusicalBase x) where
  fmap f (MusicalBase base offsets) = MusicalBase (f base) offsets

instance Foldable (MusicalBase x) where
  foldMap f (MusicalBase base offsets) = foldMap f $ (`shift` base) <$> offsets

instance Traversable (MusicalBase x) where
  traverse f (MusicalBase base offsets) = MusicalBase <$> f base <*> traverse f offsets

degree :: HasIntervals s => Degree -> Traversal' s Interval
degree deg = intervals . traverse . filtered (anyOf intervalDegree (== deg))

interval :: HasIntervals s => Interval -> Traversal' s Interval
interval int = intervals . traverse . filtered (int ==)

--isDegenerate :: (HasIntervals t) => t -> Bool
--isDegenerate music = and $ zipWith ((>) `on` intervalDegree) sortedIntervals (tail sortedIntervals)
--  where sortedIntervals = music^.intervals.to sort

noteFromInterval ::
  (HasRoot (f a) a, HasIntervals (f a), Transpose a) =>
  f a ->
  Interval ->
  Maybe a
noteFromInterval fa i =
  guard (anyOf (intervals . traverse) (i ==) fa)
    $> shift i (fa ^. root)

-- given an interval, return the note in that scale
intervalFromNote ::
  (HasRoot s a, HasIntervals s, Semitones a, Transpose a) =>
  s ->
  a ->
  Maybe Interval
intervalFromNote fa a =
  findOf (intervals . traverse) (\i -> steps (shift i (fa ^. root)) == steps a) fa

note ::
  (HasRoot (f a) a, HasIntervals (f a), Transpose a) =>
  Traversal' (f a) Interval ->
  Fold (f a) a
note l = to getNotes . traverse
  where
    getNotes fa = do
      i <- toListOf l fa
      maybeToList (noteFromInterval fa i)
