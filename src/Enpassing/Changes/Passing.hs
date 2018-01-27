{-# LANGUAGE FlexibleInstances #-}

module Enpassing.Changes.Passing where

import           Control.Comonad
import           Data.Algebra.Boolean
import           Data.Functor.Compose
import           Data.Functor.Contravariant
import           Data.Maybe
import           Data.Semigroup
import           Enpassing.Changes.Predicate
import           Enpassing.Instances
import           Enpassing.Music
import           Euterpea.Music
import           Test.QuickCheck
import           Test.QuickCheck.Gen

{-
generate_passing_chords :: Sheet -> IO Sheet
generate_passing_chords (Sheet name key bars) = generate $ Sheet name key <$> new_bars
  where new_bars = fmap as_bars . passing_chords $ Keyed key (concat bars)
-}
data PassingChord = PassingChord {
  pass_name         :: String,
  pass_situtational :: Key -> Primitive Chord -> Chord -> Bool,
  pass_generator    :: Key -> Primitive Chord -> Chord -> Gen [Primitive Chord] }

basic_passing_chord :: String
                    -> (Key -> Chord -> Chord -> Bool)
                    -> (Key -> Chord -> Chord -> Maybe Chord)
                    -> PassingChord
basic_passing_chord name pred gen = PassingChord name p2 g2
  where
    p2 key prim c2 = case prim of
      Note d c1 -> pred key c1 c2
      Rest d    -> False

    g2 key prim c2 = pure $ fromMaybe [prim] new_passing
      where
        new_passing = do
          Note d c1 <- get_note prim
          passing <- gen key c1 c2
          return [Note (d/2) c1, Note (d/2) passing]

no_addition :: PassingChord
no_addition = basic_passing_chord "No Addition" true (\_ _ _ -> Nothing)

tritone_addition :: PassingChord
tritone_addition = basic_passing_chord "Tritone Addition" true gen
  where gen _ (Chord root _ _) _ = Just . transpose_chord 6 $ Chord root Mixolydian [Add 7]
