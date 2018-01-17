{-# LANGUAGE DeriveFunctor #-}

module Enpassing.Music.Key where

import           Control.Comonad
import           Euterpea.Music

type Key = (PitchClass, Mode)
data Keyed a = Keyed Key a deriving (Functor)

instance Comonad Keyed where
  extract (Keyed k x) = x
  extend f keyed@(Keyed k _) =  Keyed k $ f keyed
