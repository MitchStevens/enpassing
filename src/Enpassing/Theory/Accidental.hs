{-# LANGUAGE DeriveFunctor #-}
module Enpassing.Theory.Accidental where

import           Control.Lens
import           Data.Functor (($>))

data Accidental a = Flat a | Natural a | Sharp a
  deriving (Eq, Show, Ord, Functor)

_Accidental :: Lens (Accidental a) (Accidental b) a b
_Accidental = lens (\case { Sharp x -> x; Natural x -> x; Flat x -> x }) ($>)

accShift :: Accidental a -> Int
accShift = \case { Flat _ -> -1; Natural _ -> 0; Sharp _ -> 1 }
