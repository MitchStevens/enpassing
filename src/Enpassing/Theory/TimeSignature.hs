module Enpassing.Theory.TimeSignature where

import           Enpassing.Theory.Types

import           Control.Monad

timeSignature :: Int -> Int -> Maybe TimeSignature
timeSignature x y = do
  guard (x > 0)
  guard (y > 0 && x `elem` [2, 4, 8, 16])
  pure $ TimeSignature x y

