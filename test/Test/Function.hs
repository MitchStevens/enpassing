-- |

module Test.Function where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

specInverse :: forall a.
  ( Eq a
  , Show a
  , Arbitrary a )
  => (a -> a) -> Spec
specInverse f = prop "" $ \x -> f (f x) === x

specAssociativity :: forall a.
  ( Eq a
  , Show a
  , Arbitrary a )
  => (a -> a -> a) -> Spec
specAssociativity f =
  prop "" $ \x y z -> f x (f y z) == f (f x y) z

specCommutative :: forall a b.
  ( Eq b
  , Show a, Show b
  , Arbitrary a )
  => (a -> a -> b) -> Spec
specCommutative f = prop "" $ \x y -> f x y === f y x

specReflexivity :: forall a.
  ( Eq a
  , Show a
  , Arbitrary a )
  => (a -> a -> Bool) -> Spec
specReflexivity f = prop "reflexivity" $ \x -> f x x === True

specSymmetry :: forall a.
  ( Eq a
  , Show a
  , Arbitrary a )
  => (a -> a -> Bool) -> Spec
specSymmetry f = prop "symmetry" $ \x y -> f x y === f y x

specTransitivity :: forall a.
  ( Eq a
  , Show a
  , Arbitrary a )
  => (a -> a -> Bool) -> Spec
specTransitivity f = prop "transitivity" $ \x y z -> if f x y && f y z then f x z else True


specRelation :: forall a.
  ( Eq a
  , Show a
  , Arbitrary a )
  => (a -> a -> Bool) -> Spec
specRelation f =
  specReflexivity f  *> specSymmetry f *> specTransitivity f
