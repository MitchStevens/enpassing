{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE UndecidableInstances   #-}

module Enpassing.Changes.JazzChange where

import           Control.Monad
import           Data.Algebra.Boolean
import           Enpassing.Changes.Interpreted
import           Enpassing.Changes.Substitution
import           Enpassing.Music
import           Prelude                        hiding (or)
import           Test.QuickCheck.Gen

class JazzChange change y z | change y -> z where
  name :: change -> String
  is_situational :: change -> Keyed y -> Bool
  generator :: change -> Keyed y -> Gen z


instance JazzChange Substitution Chord Chord where
  name           = sub_name
  is_situational = sub_situational
  generator      = sub_generator

instance (JazzChange Substitution a b, MonadPlus m, Traversable m) => JazzChange Substitution (m a) (m b) where
  name             = sub_name
  is_situational s = or . fmap (is_situational s) . sequenceA
  generator      s = traverse (generator s) . mfilter (is_situational s) . sequenceA -- :: m (Keyed a)

{-
instance JazzChange PassingChord (Primitive Chord, Chord) [Primitive Chord] where
instance JazzChange PassingChord [Primitive Chord] [Primitive Chord] where
-}
