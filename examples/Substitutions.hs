module Music.Theory.Substitutions where

import Control.Lens
import Data.Monoid

{-
        type Progression a = (Comonad w) => w a
        type Substitution a = Zipper a -> Gen a

        extend substitution progression :: Progression (Gen a)

        
        

-}
type Substitution a = a -> Maybe a

data WeightedSubs a = WeightedSubs
  { weights :: [(Int, Substitution a)] }

instance Eq a => Semigroup (WeightedSubs a) where
 WeightedSubs w1 <> WeightedSubs w2 = WeightedSubs (w1 <> w2)

sub :: (a -> Bool) -> (a -> a) -> Substitution a
sub p s = \a -> if p a then Just (s a) else Nothing

tritone :: Substitution Chord
tritone chord = root %~ (shift (-6))
              . qual 


