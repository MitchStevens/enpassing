{-# LANGUAGE FunctionalDependencies #-}
module Enpassing.Theory.Class where

import           Enpassing.Theory.Pitch
import           Enpassing.Theory.Types

import           Control.Lens

class Pitched p => RootClass s p | s -> p where
  root :: Lens' s p

class ModeClass s where
  mode :: Lens' s Mode

class DegreeClass f s where
  degree :: LensLike' f s Degree

class ExtendedClass a where
  exts :: Lens' a [Extension]

