module Enpassing.Changes.ChordPredicate where

import           Data.Functor.Contravariant
import           Data.Monoid
import           Enpassing.Music
import           Euterpea.Music

true :: Predicate Chord
true = Predicate $ const True

false :: Predicate Chord
false = Predicate $ const False

has_root :: PitchClass -> Predicate Chord
has_root p1 = Predicate $ \(Chord p2 _ _) -> p1 == p2

has_qual :: Quality -> Predicate Chord
has_qual q1 = Predicate $ \(Chord _ q2 _) -> q1 == q2

instance Monoid (Predicate m) where
  mempty = Predicate $ \_ -> False
  mappend (Predicate f1) (Predicate f2) = Predicate $ \x -> (f1 x || f2 x)

