{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ExplicitForAll            #-}
{-# LANGUAGE RankNTypes                #-}
module Enpassing.Transition.Change where

import           Enpassing.Theory
import           Enpassing.Transition.MusicM

import           Control.Arrow               ((&&&))
import           Control.Lens
import           Control.Monad.Identity
import           Data.Foldable
import           Test.QuickCheck.Gen

-- This wrapper is required to solve an ImpredicitveTypes Issue, creating
--   type Change c = forall f. Foldable f => ChangeF f c
-- doesn't typecheck.
data Change c = forall f. Foldable f => Change (ChangeF c f)

runChange :: (forall f. Foldable f => ChangeF c f -> r) -> Change c -> r
runChange f (Change change) = f change

data ChangeF c f = ChangeF
  { isValid   :: MusicM c -> Bool
  , generator :: MusicM c -> Gen (f (Duration, c)) }

instance Semigroup (ChangeF c f) where
  c1 <> c2 = ChangeF ((&&) <$> isValid c1 <*> isValid c2) gen
    where
      gen k = case (isValid c1 k, isValid c2 k) of
        (True,  True)  -> oneof [generator c1 k, generator c2 k]
        (True,  False) -> generator c1 k
        (False, True)  -> generator c2 k
        _ -> error "Generator should only be called if one of the input is valid"

instance Monoid (ChangeF x f) where
  mempty = ChangeF (const False) undefined

data Pair x = Pair x x
instance Foldable Pair where
  foldr f z (Pair x y) = x `f` (y `f` z)

type Substitution c = ChangeF c Identity
type Passing c = ChangeF c Pair

noSubstitution :: Change c
noSubstitution =
  let gen musicM = pure .Identity $ (musicM ^. duration, musicM ^. _2)
  in  Change $ ChangeF (const True) gen

genChange :: Change c -> MusicM c -> Gen [(Duration, c)]
genChange change chord =
  if runChange (`isValid` chord) change
    then runChange (\c -> fmap toList (generator c chord)) change
    else undefined

{-
runChange :: Change c -> MusicM c -> Gen [(Duration, c)]
runChange (Change change) chord =
  if isValid change chord
    then toList <$> generator change chord
    else toList <$> generator noSubstitution chord
-}
