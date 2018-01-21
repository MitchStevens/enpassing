module Enpassing.Changes.ChordPredicate where

import           Data.Algebra.Boolean
import           Data.Functor.Contravariant
import           Data.Maybe
import           Data.Monoid
import           Enpassing.Changes.Interpreted
import           Enpassing.Music
import           Euterpea.Music
import           Prelude                       hiding (not, (&&), (||))

has_root :: PitchClass -> Predicate (Keyed Chord)
has_root p1 = Predicate $ \(Keyed _ (Chord p2 _ _)) -> p1 == p2

has_qual :: Quality -> Predicate (Keyed Chord)
has_qual q1 = Predicate $ \(Keyed _ (Chord _ q2 _)) -> q1 == q2

is_interpreted :: Predicate (Keyed Chord)
is_interpreted = Predicate $ \(Keyed k (Chord root _ _)) -> isJust $ scale_degree k root

has_degree :: ScaleDegree -> Predicate (Keyed InterpretedChord)
has_degree deg1 = Predicate $ \(Keyed _ (InterpretedChord deg2 _ _)) -> deg1 == deg2

instance Boolean (Predicate a) where
  true  = Predicate $ const True
  false = Predicate $ const False
  not (Predicate p) = Predicate $ not . p
  Predicate p1 || Predicate p2 = Predicate $ \x -> p1 x || p2 x
  Predicate p1 && Predicate p2 = Predicate $ \x -> p1 x && p2 x
