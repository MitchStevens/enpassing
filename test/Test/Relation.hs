module Test.Relation where

type Relation t = t -> t -> Bool

specReflectivity :: Relation t -> Spec
specReflectivity relation = prop "" $
  \s -> relation s s

specSymmetry :: Relation t -> Spec
specSymmetry relation = prop "" $
  \s t -> relation s t == relation t s

specTransitivity :: Relation t -> Spec
specTransitivity relation = prop "" $
  \s t u -> (relation s t && relation t u) ==> relation s t

specRelation :: Relation t -> Spec
specRelation relation = describe "" $ do
  specReflectivity relation
  specSymmetry relation
  specTransitivity relation
