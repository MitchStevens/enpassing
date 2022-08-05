-- | 

module Hand where

import Data.Maybe

data HandF a = Hand
  { thumb  :: Maybe a
  , index  :: Maybe a
  , middle :: Maybe a
  , ring   :: Maybe a
  , pinky  :: Maybe a
  }
instance Functor HandF where
  fmap f = fromList . (fmap.fmap) f . toList

instance Foldable HandF where
  foldMap f = (foldMap.foldMap) f . toList

instance Traversable HandF where
  traverse f = fmap fromList . (traverse.traverse) f . toList

fromList :: [Maybe a] -> HandF a
fromList [t, i, m, r, p] =
  Hand t i m r p

toList :: HandF a -> [Maybe a]
toList (Hand t i m r p) =
  [t, i, m, r, p]

  

