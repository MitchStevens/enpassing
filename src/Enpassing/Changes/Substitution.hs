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
import           Data.Functor.Compose
import           Data.Functor.Contravariant
import           Data.List                   (intersperse)
import           Data.Maybe
import           Data.Semigroup
import           Enpassing.Changes.ChordLike
import           Enpassing.Music
import           Euterpea.Music
import           Test.QuickCheck
{-
generate_substitutions :: Sheet -> IO Sheet
generate_substitutions (Sheet name key bars) = generate $ Sheet name key <$> new_bars
  where new_bars = substitute $ Keyed key bars :: Gen [Bar]
-}
{-
  A substitution is a datastructure describing a possible chord substitution. If a chord satisfies a condition, return Just a generator for that chord, else return Nothing.

  A generator has the option

  A list of substitions is a substitution itself

  if
-}
{-|
  A substitution is a way of turning a `Keyed c` into a variant of a c, where c is chord-like.

|-}
class Substitution s c where
  -- | The name of the substitution.
  sub_name :: s -> String

  -- | Returns True if is situationally appropriate for this chord, else False.
  is_situational :: s -> Keyed c -> Bool

  -- | It may be that there are many substitutions that are appropriate for a given chord. If this is the case, then we need a way knowing how often to use each substitution.
  freq :: s -> Int

  -- |
  generator  :: s -> Keyed c -> Gen c

  -- |
  substitute :: s -> Keyed Chord -> IO Chord

instance (Substitution s c, MonadPlus m, Foldable m) => Substitution s (m c) where
  sub_name mp = intersperse ", " names
    where names = sub_name <$> toList mp

  is_situational mp chord = or . fmap (flip is_situational chord) mp

  freq = sum . fmap freq

  generator mp chord = frequency $ fmap (flip gen_tuple chord) (situationals mp chord)
    where
      gen_tuple :: s -> Keyed c -> (Int, Gen c)
      gen_tuple s c = (freq s, generator s c)

  --
  substitute mp chord = undefined

situationals :: s -> Keyed c -> m c
situationals s c = mfilter (flip is_situational c) s

data BasicSub a = BasicSub { basic_sub_name  :: String,
                             basic_freq      :: Int,
                             predicate       :: ChordPredicate (Keyed a),
                             basic_generator :: Keyed a -> Gen a }

instance Substitution (BasicSub c) c where
  sub_name = basic_sub_name
  is_situational chord = (getPredicate predicate chord) && (can_convert_to chord)
  freq = basic_freq
  generator s chord = basic_generator
  substitute s chord = generate $ generator s chord


-- Substitutions
no_sub :: BasicSub Chord
no_sub = BasicSub "No Sub" 40 (Just . pure . unkeyed)

tritone_sub :: BasicSub Chord
tritone_sub = BasicSub "Tritone" 20 (Just . tritone)
  where tritone (Keyed _ (Chord root _ _)) = pure $ Chord (fst . pitch $ pcToInt root + 6) Dom [Add 7]

--Extra instances for Primitive
instance Foldable Primitive where
  foldMap f (Note _ x) = f x
  foldMap _ _          = mempty

instance Traversable Primitive where
  traverse f (Note d x) = Note d <$> f x
  traverse f (Rest d)   = pure $ Rest d
