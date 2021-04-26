module Test.Music.Theory.Degree (
  tests
) where

import Data.GenValidity
import Test.QuickCheck
import Test.Hspec
import Test.QuickCheck.Gen

import Music.Theory.Degree

tests :: Spec
tests = pure ()

instance Validity Degree where
  validate d = check (I <= d && d <= XIII) "degree not in range"

instance Arbitrary Degree where 
  arbitrary = Degree <$> choose (0, 13)

--hunitTests :: IO ()
--hunitTests = void . runTestTT . TestLabel "Degree" $ TestList
--  [ testShowRoman
--  , testShowLowerRoman
--  , testShowUpperRoman ]
--
--testShow :: (Eq a, Parseable a) => a -> String -> Test
--testShow a str = unparse a ~=? str
--
--testShowRoman :: Test
--testShowRoman = TestList $
--  zipWith testShow oneToSeven upperRomanStrs
--
--testShowLowerRoman :: Test
--testShowLowerRoman = TestList $
--  zipWith testShow (Lower <$> oneToSeven) lowerRomanStrs
--
--testShowUpperRoman :: Test
--testShowUpperRoman = TestList $
--  zipWith testShow (Upper <$> oneToSeven) upperRomanStrs
