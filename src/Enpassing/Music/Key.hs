{-# LANGUAGE DeriveFunctor #-}

module Enpassing.Music.Key where

import Control.Comonad
import Euterpea.Music

type Key = (PitchClass, Mode)
data Keyed a = Keyed Key a deriving (Functor)


add_key :: Key -> a -> (Key, a)
add_key = (,)
