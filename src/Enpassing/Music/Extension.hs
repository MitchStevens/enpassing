{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Enpassing.Music.Extension where

import           Control.DeepSeq
import           GHC.Generics    (Generic)

{- An Extension is an extra note that can be added to a chord, to provide more colour or tension. If the note is all  -}
data Extension = Sharp Int | Flat Int | Add Int -- | No a | Sus a
  deriving (Eq, Generic, NFData)

instance Show Extension where
  show (Sharp n) = "(#"++ show n ++")"
  show (Flat  n) = "(b"++ show n ++ ")"
  show (Add   n) = if n==7 then "7" else "(add"++ show n ++")"

degree :: Extension -> Int
degree (Sharp n) = n
degree (Flat n)  = n
degree (Add n)   = n
