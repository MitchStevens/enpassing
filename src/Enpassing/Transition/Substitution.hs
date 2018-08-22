{-# LANGUAGE ExplicitForAll        #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}

module Enpassing.Transition.Substitution (
  Substitution,
{-
  no_sub,
  sub_7th,
  sub_9th,
  sub_11th,
  sub_13th,
  sub_tritone,
  sub_relative,
  sub_secondary_dominant,
  sub_simplify -}
) where

import           Prelude                        hiding (and, not, or, (&&),
                                                 (||))

import           Enpassing.Theory
import           Enpassing.Transition.Change
import           Enpassing.Transition.Predicate

import           Control.Comonad
import           Control.Monad
import           Data.Functor.Compose
--import           Data.Functor.Contravariant
import           Data.List                      (intercalate)
import           Data.Maybe
import           Data.Monoid

import           Test.QuickCheck

{-
  A substitution is a datastructure describing a possible chord substitution.
-}
type Substutition = Change Int
{-

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
no_sub, sub_7th, sub_9th, sub_11th, sub_13th, sub_tritone, sub_relative, sub_secondary_dominant, sub_simplify :: Substitution

no_sub = Substitution "No Sub" true extract

sub_7th = interpreted_substitution "Sub 7th" true gen
  where
    gen (Keyed k (InterpretedChord deg mode exts)) =
      if not (elem (Add 7) exts)
        then InterpretedChord deg mode ((Add 7):exts)
        else InterpretedChord deg mode exts

sub_9th = interpreted_substitution "Sub 9th" true gen
  where
    gen (Keyed k (InterpretedChord deg mode exts)) =
      if not (elem (Add 9) exts)
        then InterpretedChord deg mode ((Add 9):exts)
        else InterpretedChord deg mode exts

sub_11th = interpreted_substitution "Sub 11th" true gen
  where
    gen (Keyed k (InterpretedChord deg mode exts)) =
      if not (elem (Add 11) exts)
        then InterpretedChord deg mode ((Add 11):exts)
        else InterpretedChord deg mode exts

sub_13th = interpreted_substitution "Sub 13th" true gen
  where
    gen (Keyed k (InterpretedChord deg mode exts)) =
      if not (elem (Add 7) exts)
        then InterpretedChord deg mode ((Add 13):exts)
        else InterpretedChord deg mode exts

sub_tritone = Substitution "Tritone Sub" true gen
  where
    gen (Keyed k (Chord root _ _)) = transpose_chord 6 $ Chord root Mixolydian [Add 7]

sub_relative = interpreted_substitution "Sub Relative" true gen
  where
    gen (Keyed k (InterpretedChord deg mode exts)) = case (deg, mode) of
      (I,   Major) -> InterpretedChord VI  Minor exts
      (II,  Major) -> InterpretedChord IV  Major exts
      (III, Major) -> InterpretedChord V   Major exts
      (IV,  Major) -> InterpretedChord II  Minor exts
      (V,   Major) -> InterpretedChord III Minor exts
      (VI,  Major) -> InterpretedChord I   Minor exts
      (VII, Major) -> InterpretedChord V   Mixolydian exts

-- TODO: finish this substitution
sub_secondary_dominant = Substitution "Sub Secondary Dominant" false extract

sub_simplify = interpreted_substitution "Simplify" true gen
  where
    gen (Keyed k (InterpretedChord deg mode exts)) = case mode of
      CustomMode "Dim" -> InterpretedChord deg Minor []
      CustomMode "Aug" -> InterpretedChord deg Major []
      _                -> InterpretedChord deg mode  []

{-
sharp_i_replaces_VI :: Substitution
sharp_i_replaces_VI = interpreted_substitution
                        "#i dim replaces vi"
                        (has_degree IV . extract)
                        gen
  where
    gen :: Keyed InterpretedChord -> Chord
    gen kc@(Keyed k _) = transpose_chord 1 . as_chord $ Keyed k chord
      where chord = Chord (degree_as_pitch (Keyed k I)) (CustomMode "Dim") [Add 7]
-}
-}
