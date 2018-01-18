{-# LANGUAGE ExplicitForAll         #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE UndecidableInstances   #-}

module Enpassing.Changes.Substitution (
  Substitution,
  generate_substitutions
) where

import           Control.Comonad
import           Control.Monad
import           Data.Algebra.Boolean
import           Data.Functor.Compose
import           Data.Functor.Contravariant
import           Data.List                        (intercalate)
import           Data.Maybe
import           Data.Monoid
import           Enpassing.Changes.ChordLike
import           Enpassing.Changes.ChordPredicate as ChordPredicate
import           Enpassing.Changes.Interpreted
import           Enpassing.Music
import           Euterpea.Music
import           Prelude                          hiding (and, not, or, (&&),
                                                   (||))
import           Test.QuickCheck

generate_substitutions :: Sheet -> IO Sheet
generate_substitutions (Sheet name key bars) = Sheet name key <$> substitute (Keyed key bars)
  where
    substitute_list :: (Traversable t) => Keyed (t Chord) -> IO (t Chord)
    substitute_list (Keyed k trav) = mapM (substitute . Keyed k) trav

class Substitutable s where
  substitute :: Keyed s -> IO s

instance Substitutable Chord where
  substitute chord = generate $ generator (mconcat subs) chord
    where subs =[no_sub, tritone_sub, sharp_i_replaces_VI]

instance (Traversable t, Substitutable s) => Substitutable (t s) where
  substitute (Keyed k trav) = mapM (substitute . Keyed k) trav

{-
  A substitution is a datastructure describing a possible chord substitution.
-}
data Substitution = Substitution {
  -- | The name of the substitution.
  sub_name       :: String,

  -- | Returns True if is situationally appropriate for this chord, else False.
  is_situational :: Predicate (Keyed Chord),

  -- | It may be that there are many substitutions that are appropriate for a given chord. If this is the case, then we need a way knowing how often to use each substitution.
  freq           :: Int,

  -- |
  generator      :: Keyed Chord -> Gen Chord }

instance Monoid Substitution where
  mempty = undefined

  mappend s1 s2 = mconcat [s1, s2]

  mconcat list = Substitution name pred d gen
    where
      name = intercalate ", " $ fmap sub_name list
      pred = or $ fmap is_situational list
      d    = sum $ fmap freq list
      gen  = \chord -> frequency $ mapMaybe (gen_tuples chord) list

      gen_tuples :: Keyed Chord -> Substitution -> Maybe (Int, Gen Chord)
      gen_tuples chord (Substitution _ p d g) =
        if getPredicate p chord
          then Just (d, g chord)
          else Nothing

basic_substitution :: String
                      -> Predicate (Keyed Chord)
                      -> Int
                      -> (Keyed Chord -> Gen Chord)
                      -> Substitution
basic_substitution = Substitution

interpreted_substitution :: (ChordLike c)
                            => String
                            -> Predicate (Keyed InterpretedChord)
                            -> Int
                            -> (Keyed InterpretedChord -> Gen c)
                            -> Substitution
interpreted_substitution name pred d g = Substitution name new_pred d gen
  where gen chord@(Keyed k _) = fmap (as_chord . Keyed k) . g . extend convert_to $ chord

        new_pred = is_interpreted && contramap (extend convert_to) pred :: Predicate (Keyed Chord)

        convert_to :: Keyed Chord -> InterpretedChord
        convert_to (Keyed k (Chord root qual exts)) =
          fromJust $ (\deg -> InterpretedChord deg qual exts) <$> scale_degree k root


-- Substitutions
no_sub :: Substitution
no_sub = basic_substitution "No Sub" (ChordPredicate.true) 40 (pure . extract)

tritone_sub :: Substitution
tritone_sub = basic_substitution "Tritone" (ChordPredicate.true) 20 (pure . tritone)
  where tritone (Keyed _ (Chord root _ _)) = Chord (fst . pitch $ pcToInt root + 6) Dom [Add 7]

sharp_i_replaces_VI :: Substitution
sharp_i_replaces_VI = interpreted_substitution
                        "#i dim replaces vi"
                        (has_degree IV)
                        100
                        gen
  where
    gen :: Keyed InterpretedChord -> Gen Chord
    gen (Keyed k (InterpretedChord _ qual exts)) =
      pure . transpose_chord 1 . as_chord . Keyed k $ InterpretedChord I Dim [Add 7]



--Extra instances for Primitive
instance Foldable Primitive where
  foldMap f (Note _ x) = f x
  foldMap _ _          = mempty

instance Traversable Primitive where
  traverse f (Note d x) = Note d <$> f x
  traverse f (Rest d)   = pure $ Rest d


