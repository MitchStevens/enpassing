{-# LANGUAGE DeriveFunctor, FlexibleInstances, DeriveGeneric, DeriveAnyClass #-}

module Enpassing.Music.Extension where

import Control.DeepSeq
import GHC.Generics (Generic)
import Test.QuickCheck

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

instance Arbitrary Extension where
  arbitrary = modifier <*> elements [7, 9, 11, 13]
    where modifier = frequency $ zip [5, 2, 1] $ map pure [Add, Sharp, Flat]