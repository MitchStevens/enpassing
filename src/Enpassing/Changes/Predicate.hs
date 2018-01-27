module Enpassing.Changes.Predicate where

import           Control.Comonad
import           Data.Algebra.Boolean
import           Data.Maybe
import           Enpassing.Changes.Interpreted
import           Enpassing.Music
import           Euterpea.Music
import           Prelude                       hiding (not, (&&), (||))

{-
instances for Boolean (x -> b),
              Boolean (x -> y -> b),
              Boolean (x -> y -> z -> b), ...
-}
instance (Boolean b) => Boolean (a -> b) where
  true  = const true
  false = const false
  not f = not . f
  (f || g) x = f x || g x
  (f && g) x = f x && g x



-- Substution Predicates

{-| check the  |-}
has_root :: PitchClass -> Keyed Chord -> Bool
has_root p1 = (p1 ==) . root . extract

{-| The Keyed |-}
has_mode :: Mode -> Keyed Chord -> Bool
has_mode m1 = (m1 ==) . mode . extract

{-| |-}
is_interpreted :: Keyed Chord -> Bool
is_interpreted = isJust . pitch_as_degree . fmap root

{-| |-}
has_degree :: ScaleDegree -> InterpretedChord -> Bool
has_degree deg1 (InterpretedChord deg2 _ _) = deg1 == deg2



-- Passing Chord Predicate

{-| |-}
no_copy :: Chord -> Chord
no_copy = undefined
