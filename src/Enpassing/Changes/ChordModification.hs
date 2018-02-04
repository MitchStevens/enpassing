module Enpassing.Changes.ChordModification (
  Modification (..),
  modify
) where

import Enpassing.Music
import Euterpea.Music
import Test.QuickCheck.Gen
import Control.Comonad
import Control.Monad

class Modification m where
  name :: m -> String
  situational :: m -> Keyed (Primitive Chord) -> Bool
  generator   :: m -> Keyed (Primitive Chord) -> Gen [Primitive Chord]

  create :: m -> Keyed (Primitive Chord) -> IO [Primitive Chord]
  create m chord = if situational m chord
                     then generate (generator m chord)
                     else pure [extract chord]

modify :: Modification m => m -> Sheet -> IO Sheet
modify m (Sheet name key bars) = Sheet name key <$> new_bars
  where new_bars = fmap concat $ mapM (create m . Keyed key) bars