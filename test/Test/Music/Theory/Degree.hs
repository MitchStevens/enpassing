module Test.Music.Theory.Degree (
  tests
) where

import Music.Theory.Degree

import Test.Hspec
import Test.QuickCheck

tests :: Spec
tests = describe "Degree" $ do
  pure ()

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
--oneToSeven = [d1, d2, d3, d4, d5, d6, d7]
--upperRomanStrs = ["I", "II", "III", "IV", "V", "VI", "VII"]
--lowerRomanStrs = ["i", "ii", "iii", "iv", "v", "vi", "vii"]
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
