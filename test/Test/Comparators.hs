module Test.Comparators where

import           Test.HUnit
import           Test.QuickCheck
import           Text.Printf

class TestComparators test where
  (.>)  :: (Show a, Ord a) => a -> a -> test
  (.>=) :: (Show a, Ord a) => a -> a -> test
  (.<)  :: (Show a, Ord a) => a -> a -> test
  (.<=) :: (Show a, Ord a) => a -> a -> test

instance TestComparators Test where
  (.>)  x y = TestCase $ assertBool (printf "found %s <= %s" (show x) (show y)) (x > y)
  (.>=) x y = TestCase $ assertBool (printf "found %s < %s"  (show x) (show y)) (x >= y)
  (.<)  x y = TestCase $ assertBool (printf "found %s >= %s" (show x) (show y)) (x < y)
  (.<=) x y = TestCase $ assertBool (printf "found %s > %s"  (show x) (show y)) (x <= y)

instance TestComparators Property where
  (.>)  x y = counterexample (printf "found %s <= %s" (show x) (show y)) (x > y)
  (.>=) x y = counterexample (printf "found %s < %s"  (show x) (show y)) (x >= y)
  (.<)  x y = counterexample (printf "found %s >= %s" (show x) (show y)) (x < y)
  (.<=) x y = counterexample (printf "found %s > %s"  (show x) (show y)) (x <= y)
