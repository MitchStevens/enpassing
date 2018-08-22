{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Enpassing.Theory.Key where

import           Enpassing.Theory.Pitch
import           Enpassing.Theory.Types

import           Control.Comonad
import           Control.Lens

keyed :: Key -> a -> (Key, a)
keyed = (,)

class Keyed w a where
  key :: Lens' (w a) Key

instance Keyed ((,) Key) a where
  key = _1

