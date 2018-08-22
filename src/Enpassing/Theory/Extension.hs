{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Enpassing.Theory.Extension where

import           Enpassing.Theory.Accidental
import           Enpassing.Theory.Class
import           Enpassing.Theory.Degree
import           Enpassing.Theory.Types

import           Control.Lens
import           Data.Ord                    (comparing)
import           Data.Proxy                  (Proxy)

instance Ord Extension where
  compare = comparing extOrder
   where
    extOrder :: Extension -> Int
    extOrder = \case
      Acc (Natural deg) -> case review mkDegree deg of
        6  -> 6
        7  -> 7
        9  -> 9
        11 -> 11
        13 -> 13
        n  -> 100 + n
      Sus2              -> 50
      Sus4              -> 50
      Acc (Sharp deg)   -> review mkDegree deg + 200
      Acc (Flat deg)    -> review mkDegree deg + 200
      No deg            -> review mkDegree deg + 300

instance (Functor f, Contravariant f) => DegreeClass f Extension where
  degree = to $ \case
    Acc acc -> acc ^. _Accidental
    Sus2    -> d2
    Sus4    -> d4
    No n    -> n
