{-# LANGUAGE FlexibleInstances #-}

module Enpassing.Changes.Passing (
  Passing,
  passing,

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
  , pas_gen  :: Keyed Chord -> Chord
  }

instance Modification Passing where
  name = pas_name
  situational m (Keyed k prim) = case prim of
    Note d x -> if d >= (1%4) then pas_pred m (Keyed k x) else False
    Rest _   -> False
  generator m (Keyed k prim) = case prim of
    Note d c1 -> pure [Note (d/2) c1, Note (d/2) c2]
      where c2 = pas_gen m (Keyed k c2)
    Rest _    -> error "Can't add a passing chord to a rest"

passing :: String
        -> (Keyed Chord -> Bool)
        -> (Keyed Chord -> Chord)
        -> Passing
passing = Passing

no_addition :: Passing
no_addition = Passing "No Addition" true extract

tritone_addition :: Passing
tritone_addition = Passing "Tritone Addition" true gen
  where gen (Keyed k (Chord root _ _)) = transpose_chord 6 $ Chord root Mixolydian [Add 7]
