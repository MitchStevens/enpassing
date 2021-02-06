module Music.Theory.Classes where

import Control.Lens

import Music.Theory.Quality

class HasRoot s a where
  root :: Lens' s a

class HasQuality s where
  qual :: Traversal' s Quality
