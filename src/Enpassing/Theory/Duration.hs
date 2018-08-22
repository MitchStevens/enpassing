{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Enpassing.Theory.Duration where

import           Enpassing.Theory.Types

import           Control.Lens

temporal :: Duration -> a -> (Duration, a)
temporal = (,)

class Temporal w a where
  duration :: Lens' (w a) Duration
instance Temporal ((,) Duration) a where
  duration = _1
