{-# LANGUAGE FlexibleInstances #-}

module Enpassing.Changes.Passing where

import           Control.Comonad
import           Data.Functor.Compose
import           Data.Functor.Contravariant
import           Data.Functor.Contravariant
import           Data.Maybe
import           Data.Semigroup
import           Data.Semigroup
import           Enpassing.Changes.ChordPredicate as ChordPredicate
import           Enpassing.Music
import           Enpassing.Music
import           Euterpea.Music
import           Euterpea.Music
import           Test.QuickCheck
import           Test.QuickCheck.Gen

class AddPassing p where
  passing_chords :: Keyed p -> Gen p

instance AddPassing [Primitive Chord] where
  passing_chords (Keyed key list) = sequenceA $ concatMap passing_chord tuples
    where
      PassingAddition _ _ p gen = no_addition <> tritone_addition
      tuples = zip list (tail $ cycle list)

      passing_chord :: (Primitive Chord, Primitive Chord) -> [Gen (Primitive Chord)]
      passing_chord (Note d x1, Note _ x2) = [pure (Note (d/2) x1), Note (d/2) <$> gen key x1 x2]
      passing_chord (Note d x1, Rest _)    = [pure (Note d x1)]
      passing_chord (Rest d,    _)         = [pure (Rest d)]

generate_passing_chords :: Sheet -> IO Sheet
generate_passing_chords (Sheet name key bars) = generate $ Sheet name key <$> new_bars
  where new_bars = fmap as_bars . passing_chords $ Keyed key (concat bars)

data PassingAddition = PassingAddition {
  add_name  :: String,
  add_prob  :: Int,
  predicate :: Predicate (Keyed (Primitive Chord)),
  generator :: Key -> Chord -> Chord -> Gen Chord }

instance Semigroup PassingAddition where
  PassingAddition n1 d1 p1 g1 <> PassingAddition n2 d2 p2 g2 = PassingAddition n3 d3 p3 g3
    where
      n3 = n1++", "++n2
      d3 = d1 + d2
      p3 = mappend p1 p2
      g3 k c1 c2 = frequency [(d1, g1 k c1 c2), (d2, g2 k c1 c2)]

no_addition :: PassingAddition
no_addition = PassingAddition "No Addition" 40 (Predicate $ const True) (\_ c _ -> pure c)

tritone_addition :: PassingAddition
tritone_addition = PassingAddition "Tritone Addition" 20 (Predicate $ const True) f
  where f _ (Chord root _ _) _ = pure $ Chord (fst . pitch $ pcToInt root + 6) Dom [Add 7]
