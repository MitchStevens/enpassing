module Enpassing.Instances where

import           Euterpea.Music

-- Primitive Instaces
instance Foldable Primitive where
  foldMap f (Note _ x) = f x
  foldMap _ _          = mempty

instance Traversable Primitive where
  traverse f (Note d x) = Note d <$> f x
  traverse f (Rest d)   = pure $ Rest d

get_note :: Primitive a -> Maybe (Primitive a)
get_note (Note d x) = Just $ Note d x
get_note (Rest _)   = Nothing
