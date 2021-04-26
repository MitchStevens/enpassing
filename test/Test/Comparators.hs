module Test.Comparators where

import           Test.QuickCheck
import           Text.Printf

--infixr 1 .>
--infixr 1 .>=
--infixr 1 .< 
--infixr 1 .<=
--
--(.>) :: (Ord a, Show a) => a -> a -> Property
--(.>)  x y = counterexample (printf "found %s <= %s" (show x) (show y)) (x > y)
--
--(.>=) :: (Ord a, Show a) => a -> a -> Property
--(.>=) x y = counterexample (printf "found %s < %s"  (show x) (show y)) (x >= y)
--
--(.<) :: (Ord a, Show a) => a -> a -> Property
--(.<)  x y = counterexample (printf "found %s >= %s" (show x) (show y)) (x < y)
--
--(.<=) :: (Ord a, Show a) => a -> a -> Property
--(.<=) x y = counterexample (printf "found %s > %s"  (show x) (show y)) (x <= y)
