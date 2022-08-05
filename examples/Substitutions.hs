module Music.Theory.Substitutions where

import Control.Lens
import Data.Monoid

{-
        type Progression a = (Comonad w) => w a
        type Substitution a = Zipper a -> Gen a

        extend substitution progression :: Progression (Gen a)

  types of subs:
    Chord -> Chord
    Chord -> Gen Chord
    Chord -> Weighted Chord
    f Chord -> Chord

    Substitution = (Comonad f, Monad g) => f a -> g a

-}
data Weighted a = Weighted
  { weights :: [(Int, a)] }

type Substitution f a = f a -> Weighted a
-- type Substitution = SubstitutionB Zipper

instance (Eq a, Ord a) => Semigroup (Weighted a) where
  Weighted w1 <> Weighted w2 = Weighted (mergeWeighted w1 w2)

instance Monoid (Weighted a) where
  mempty = Weighted []

mergeWeighted :: (Eq a, Ord a) => [(Int, a)] -> [(Int, a)] -> [(Int, a)]
mergeWeighted [] w = w
mergeWeighted w [] = w
mergeWeighted w1((n1, a1):r1) w2@((n2, a2):r2) = case compare a1 a2 of
  LT -> (n1, a1) : mergeWeighted r1 w2
  EQ -> (n1+n2, a1) : mergeWeighted r1 r2
  GT -> (n2, a2) : mergeWeighted w1 r2

runSubstitution :: (Eq a, Ord a, Comonad f) => Substitution f a -> (Weighted a -> m a) -> f a -> m (f a)

sub :: (a -> Bool) -> (a -> a) -> Substitution a
sub p s = \a -> if p a then Just (s a) else Nothing

tritone :: Substitution Chord
tritone chord = root %~ (shift (-6))
              . qual
