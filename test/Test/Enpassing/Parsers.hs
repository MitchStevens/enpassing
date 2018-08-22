{-# LANGUAGE OverloadedStrings #-}
module Test.Enpassing.Parsers (
  runTests
) where

import           Prelude              hiding (zipWith)

import           Enpassing.Parsers
import           Enpassing.Theory
import           Test.Generators

import           Data.Attoparsec.Text hiding (D, I)
import           Data.Foldable
import           Data.Functor         (void)
import           Data.Text            as T
import           Data.Text            (Text, pack)
import           Debug.Trace

import           Test.HUnit
import           Test.QuickCheck
import           Text.Printf


runTests :: IO ()
runTests = do
  quickCheckTests
  hunitTests

end :: Parser ()
end = do
  mc <- peekChar
  maybe (pure ()) (fail . show) mc

-- QuickCheck Tests
quickCheckTests :: IO ()
quickCheckTests = do
  quickCheck $ prop_ShowParse (parser :: Parser Note)
  quickCheck $ prop_ShowParse (parser :: Parser Extension)
  quickCheck $ prop_ShowParse (parser :: Parser PitchClass)
  quickCheck $ prop_ShowParse (parser :: Parser Degree)
  quickCheck $ prop_ShowParse (parser :: Parser ExtendedChord)
  quickCheck $ prop_ShowParse (parser :: Parser InterpretedChord)
  quickCheck $ prop_ShowParse (parser :: Parser SlashChord)
  quickCheck $ prop_ShowParse (parser :: Parser Chord)

prop_ShowParse :: (Arbitrary a, Show a, Eq a, Parseable a) => Parser a -> a -> Property
prop_ShowParse p a =
  let a' = parseOnly (p <* end) (T.pack $ unparse a)
  in pure a === a'

--HUnit Tests
hunitTests :: IO ()
hunitTests =
  let
    Right cmaj = Extended <$> extended (natural C) Ionian  []
    Right dmb5 = Extended <$> extended (natural D) Aeolian [Acc (Natural d7), Acc (Flat d5)]
  in void . runTestTT $ TestList
    [ testChordParsing "C"         cmaj
    , testChordParsing "CΔ"        cmaj
    , testChordParsing "Cmaj"      cmaj
    , testChordParsing "Dm7(b5)"   dmb5
    , testChordParsing "Dmin7(b5)" dmb5
    , testChordParsing "D-7(b5)"   dmb5
    ]

testChordParsing :: String -> Chord -> Test
testChordParsing str chord =
  let chord' = parseOnly (parser <* end) (T.pack str)
  in chord' ~?= pure chord
