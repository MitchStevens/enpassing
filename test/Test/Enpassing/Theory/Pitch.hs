module Test.Enpassing.Theory.Pitch where

import           Enpassing.Theory
import           Test.Generators

import           Data.Function    (on)
import           Data.Functor     (void)
import           Data.List        (sort)
import           Test.Comparators
import           Test.HUnit
import           Test.QuickCheck
import           Text.Printf

runTests :: IO ()
runTests = do
  quickCheckTests
  hunitTests


--QuickCheckTests
quickCheckTests :: IO ()
quickCheckTests = do
  quickCheck $ counterexample "prop_pitchClass" prop_pitchClass
  quickCheck $ counterexample "prop_flat" prop_flat
  quickCheck $ counterexample "prop_sharp" prop_sharp
  quickCheck $ counterexample "prop_PitchClassId" prop_PitchClassId
  quickCheck $ counterexample "prop_PitchId" prop_PitchId

prop_pitchClass :: Int -> Int -> Property
prop_pitchClass x y =
  (mod12 (x-y) == 0) === (pitchClass x == pitchClass y)

prop_flat :: Note -> Property
prop_flat note = note /= C ==>  (flat note .< natural note :: Property)

prop_sharp :: Note -> Property
prop_sharp note = note /= B ==> (natural note .< sharp note :: Property)

prop_PitchClassId :: PitchClass -> Property
prop_PitchClassId p =
  p === (pitchClass . semitones) p

prop_PitchId :: Pitch -> Property
prop_PitchId p =
  p === (pitch . semitones) p


--HUnit Tests
hunitTests :: IO ()
hunitTests = void . runTestTT $ TestList
  [ test_allPitchClasses ]

test_allPitchClasses :: Test
test_allPitchClasses = TestList
  [ TestList (zipWith (.<) allPitchClasses (tail allPitchClasses))
  , TestCase (assertEqual "All pitched not sorted" allPitchClasses (sort allPitchClasses)) ]
