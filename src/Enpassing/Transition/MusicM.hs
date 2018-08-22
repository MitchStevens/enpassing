{-# LANGUAGE FlexibleInstances #-}
module Enpassing.Transition.MusicM where

import           Enpassing.Theory.Chord
import           Enpassing.Theory.Duration
import           Enpassing.Theory.Key
import           Enpassing.Theory.Pitch
import           Enpassing.Theory.Types

import           Control.Lens
import           Control.Monad.State.Lazy

data MusicalContext a = MusicalContext
  { _ctxKey      :: Key
  , _ctxDuration :: Duration
  --, signature   :: Maybe TimeSignature
  , _predecessor :: Maybe a
  , _successor   :: Maybe a }
makeLenses ''MusicalContext

type MusicM a = (MusicalContext a, a)

instance Keyed ((,) (MusicalContext a)) a where
  key = _1 . ctxKey

instance Temporal ((,) (MusicalContext a)) a where
  duration = _1 . ctxDuration

type ListZipper a = ([a], a, [a])

zipperIter :: (Maybe a -> a -> Maybe a -> b) -> [a] -> [b]
zipperIter f [] = []
zipperIter f (x:xs) = iter f ([], x, xs) 
  where
    iter :: (Maybe a -> a -> Maybe a -> b) -> ListZipper a -> [b]
    iter f = \case
      ([], x, [])     -> (f Nothing x Nothing)  : []
      (a:as, x, [])   -> f (Just a) x Nothing  : []
      ([], x, b:bs)   -> f Nothing  x (Just b) : iter f ([x], b, bs)
      (a:as, x, b:bs) -> f (Just a) x (Just b) : iter f (x:a:as, b, bs)

musicM :: Key -> [(Duration, a)] -> [MusicM a]
musicM key = zipperIter toMusicM
  where
    toMusicM :: Maybe (Duration, a) -> (Duration, a) -> Maybe (Duration, a) -> MusicM a
    toMusicM pred (d, x) succ = (MusicalContext key d p s, x)
      where (p, s) = (snd <$> pred, snd <$> succ)