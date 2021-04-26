module Music.Theory.MusicalBase where

import Control.Lens
import Data.Function
import Data.List
import Data.Maybe
import Control.Monad

import Music.Theory.Degree
import Music.Theory.Interval
import Music.Theory.Classes
import Music.Theory.Transpose

data MusicalBase a = MusicalBase { _base :: !a, _offsets :: ![Interval] }
  deriving (Eq)
makeLenses ''MusicalBase

instance Functor MusicalBase where
  fmap = over base

instance HasRoot (MusicalBase a) a where
  root = base

instance HasIntervals (MusicalBase a) where
  intervals = offsets

degree :: HasIntervals s => Degree -> Traversal' s Interval
degree deg = intervals . traverse . filtered (\i -> intervalDegree i == deg)

interval :: HasIntervals s => Interval -> Traversal' s Interval
interval int = intervals . traverse . filtered (int ==)
  
isDegenerate :: (HasIntervals t) => t -> Bool
isDegenerate music = and $ zipWith ((>) `on` intervalDegree) sortedIntervals (tail sortedIntervals)
  where sortedIntervals = music^.intervals.to sort

noteFromInterval :: (HasRoot (f a) a, HasIntervals (f a), Transpose a) => f a -> Interval -> Maybe a
noteFromInterval fa i = do
  guard (anyOf (intervals.traverse) (i==) fa)
  pure (shift i (fa^.root))

intervalFromNote :: (HasRoot s a, HasIntervals s, Semitones a, Transpose a) => s -> a -> Maybe Interval
intervalFromNote fa a = findOf (intervals.traverse) (\i -> steps (shift i (fa^.root)) == steps a) fa

note :: (HasRoot (f a) a, HasIntervals (f a), Transpose a)
     => Traversal' (f a) Interval -> Fold (f a) a
note l = to getNotes . traverse . id--_a -- . traverse
  where
    getNotes fa = do
      i <- toListOf l fa
      maybeToList (noteFromInterval fa i)
