module Test.Function where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

specInverse ::
  forall a b.
  ( Eq a,
    Eq b,
    Show a,
    Show b,
    Arbitrary a,
    Arbitrary b
  ) =>
  (a -> b) ->
  (b -> a) ->
  Spec
specInverse f g =
  prop "Inverse" $
    (\x -> f (g x) === x) .&&. (\y -> g (f y) === y)

specIdempotent ::
  forall a.
  ( Eq a,
    Show a,
    Arbitrary a
  ) =>
  (a -> a) ->
  Spec
specIdempotent f = prop "Idempotent" $
  \x -> f x === f (f x)

specInvolution ::
  forall a.
  ( Eq a,
    Show a,
    Arbitrary a
  ) =>
  (a -> a) ->
  Spec
specInvolution f = prop "" $ \x -> f (f x) === x

specAssociativity ::
  forall a.
  ( Eq a,
    Show a,
    Arbitrary a
  ) =>
  (a -> a -> a) ->
  Spec
specAssociativity f =
  prop "" $ \x y z -> f x (f y z) == f (f x y) z

specCommutative ::
  forall a b.
  ( Eq b,
    Show a,
    Show b,
    Arbitrary a
  ) =>
  (a -> a -> b) ->
  Spec
specCommutative f = prop "" $ \x y -> f x y === f y x

-- relations
specReflexivity ::
  forall a.
  ( Eq a,
    Show a,
    Arbitrary a
  ) =>
  (a -> a -> Bool) ->
  Spec
specReflexivity f = prop "reflexivity" $ \x -> f x x === True

specSymmetry ::
  forall a.
  ( Eq a,
    Show a,
    Arbitrary a
  ) =>
  (a -> a -> Bool) ->
  Spec
specSymmetry f = prop "symmetry" $ \x y -> f x y === f y x

specTransitivity ::
  forall a.
  ( Eq a,
    Show a,
    Arbitrary a
  ) =>
  (a -> a -> Bool) ->
  Spec
specTransitivity f = prop "transitivity" $ \x y z -> if f x y && f y z then f x z else True

specRelation ::
  forall a.
  ( Eq a,
    Show a,
    Arbitrary a
  ) =>
  (a -> a -> Bool) ->
  Spec
specRelation f =
  specReflexivity f *> specSymmetry f *> specTransitivity f
