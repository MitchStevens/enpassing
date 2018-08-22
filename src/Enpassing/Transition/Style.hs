{-# LANGUAGE ExplicitForAll    #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes        #-}

module Enpassing.Transition.Style (
  Style (..),
  styliseSheet,
  styles
) where

import           Data.Bifunctor
import           Data.Map.Strict             (Map, fromList)
import           Data.Monoid
import           Data.Semigroup

import           Enpassing.Theory
import           Enpassing.Transition.Change

import           Test.QuickCheck.Gen

data Style c = Style
  { changes :: [ (Int, Change c) ] }

instance Semigroup (Style c) where
  Style l1 <> Style l2 =  Style (l1 <> l2)

instance Monoid (Style c) where
  mempty = Style []

styliseSheet :: Style Chord -> Sheet -> IO Sheet
styliseSheet (Style changes) sheet = undefined

styles :: Map String (Style Chord)
styles = fromList
  [ ("None",        none)
  , ("Basic Jazz",  basicJazz)
  , ("Simplify",    simplify)
  , ("Moody",       moody)]

none = undefined
basicJazz = undefined
simplify = undefined
moody = undefined

{-
Style "Basic Jazz"
  [ (100, no_sub)
  , (100, sub_7th)
  , (50,  sub_9th)
  , (10,  sub_11th)
  , (10,  sub_13th)
  , (30,  sub_tritone)
  , (30,  sub_relative)
  ] []

simplify = Style "Simplify"
  [(100, sub_simplify)]
  []

moody = Style "Moody" [] []
-}
