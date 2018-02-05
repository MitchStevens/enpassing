{-# LANGUAGE FlexibleInstances #-}

module Enpassing.Changes.Passing (
  Passing,
  simple_passing,

  no_addition,
  tritone_addition
) where

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
import Enpassing.Changes.ChordModification
import           Test.QuickCheck.Gen
import Data.Ratio

data Passing = Passing
  { pas_name :: String
  , pas_pred :: Keyed Chord -> Bool
  , pas_gen  :: Keyed Chord -> [Chord]
  }

instance Modification Passing where
  name = pas_name
  situational m (Keyed k prim) = case prim of
    Note d x -> if d < (1%4) then False
                else pas_pred m (Keyed k x)
    Rest _   -> False
  generator m (Keyed k prim) = case prim of
    Note d c -> pure . fmap note $ list
      where
        note = Note (d/n)
        n = fromInteger . toInteger $ length list :: Rational
        list = pas_gen m $ Keyed k c :: [Chord]
    Rest d   -> pure [Rest d]

simple_passing :: String
        -> (Keyed Chord -> Bool)
        -> (Keyed Chord -> Chord)
        -> Passing
simple_passing name pred gen = Passing name pred new_gen
  where
    new_gen chord@(Keyed k c1) = [c1, gen chord]

no_addition :: Passing
no_addition = Passing "No Addition" true (pure.extract)

tritone_addition :: Passing
tritone_addition = simple_passing "Tritone Addition" true gen
  where gen (Keyed k (Chord root _ _)) = transpose_chord 6 $ Chord root Mixolydian [Add 7]
