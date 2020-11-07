module PropScale where

import           Data.List       (findIndex)
import           Enpassing.Music
import           Euterpea.Music
import           Test.QuickCheck
import           TestUtils
import           Text.Printf

main = do
  putStrLn "\nPropScale tests"
  quickCheck infinite_scale_prop
  quickCheck scale_degree_prop

infinite_scale_prop :: Int -> (Pitch, Mode) -> Bool
infinite_scale_prop n s = and $ zipWith (<) scale (tail scale)
  where scale = map absPitch $ take n $ infinite_scale s

scale_degree_prop :: Keyed PitchClass -> Property
scale_degree_prop pc@(Keyed k@(p1, mode) p2) = counterexample message $
  non_dim_aug mode ==>
  maybe_degree == pitch_as_degree pc
  where
    message = printf "  - Expected: %s\n  - Actual:   %s\n  - Scale:   %s"
      (show maybe_degree)
      (show (pitch_as_degree pc))
      (show pclass_scale)
    pclass_scale = fst <$> mk_scale ((p1, 0), mode) :: [PitchClass]
    maybe_degree = toEnum <$> findIndex (\x -> pcToInt x == pcToInt p2) pclass_scale :: Maybe ScaleDegree

non_dim_aug :: Mode -> Bool
non_dim_aug mode = case mode of
  CustomMode "Dim" -> False
  CustomMode "Aug" -> False
  _                -> True
