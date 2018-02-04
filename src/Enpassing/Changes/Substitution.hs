{-# LANGUAGE ExplicitForAll        #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}

module Enpassing.Changes.Substitution where

import           Control.Comonad
import           Control.Monad
import           Data.Algebra.Boolean
import           Data.Functor.Compose
import           Data.Functor.Contravariant
import           Data.List                     (intercalate)
import           Data.Maybe
import           Data.Monoid
import           Enpassing.Changes.ChordLike
import Enpassing.Changes.ChordModification
import           Enpassing.Changes.Interpreted
import           Enpassing.Changes.Predicate
import           Enpassing.Music
import           Euterpea.Music
import           Prelude                       hiding (and, not, or, (&&), (||))
import           Test.QuickCheck

{-
  A substitution is a datastructure describing a possible chord substitution.
-}
data Substitution = Substitution 
  { sub_name :: String
  , sub_pred :: Keyed Chord -> Bool
  , sub_gen  :: Keyed Chord -> Chord
  }

instance Modification Substitution where
  name = sub_name
  situational m (Keyed k prim) = case prim of
    Note _ x -> sub_pred m (Keyed k x)
    Rest _   -> False
  generator m (Keyed k prim) = case prim of
    Note d x -> pure [Note d (sub_gen m (Keyed k x))]
    Rest _   -> error "cant substitute into a rest"

interpreted_substitution :: (ChordLike c)
                         => String
                         -> (Keyed InterpretedChord -> Bool)
                         -> (Keyed InterpretedChord -> c)
                         -> Substitution
interpreted_substitution name pred g = Substitution name new_pred gen
  where
    gen = as_interpreted =>= g =>= as_chord :: Keyed Chord -> Chord
    new_pred = is_interpreted && (as_interpreted =>= pred)



-- Substitutions
no_sub :: Substitution
no_sub = Substitution "No Sub" true extract

tritone_sub :: Substitution
tritone_sub = Substitution "Tritone" true tritone
  where tritone (Keyed _ (Chord root _ _)) = transpose_chord 6 $ Chord root Mixolydian [Add 7]

sharp_i_replaces_VI :: Substitution
sharp_i_replaces_VI = interpreted_substitution
                        "#i dim replaces vi"
                        (has_degree IV . extract)
                        gen
  where
    gen :: Keyed InterpretedChord -> Chord
    gen kc@(Keyed k _) = transpose_chord 1 . as_chord $ Keyed k chord
      where chord = Chord (degree_as_pitch (Keyed k I)) (CustomMode "Dim") [Add 7]
