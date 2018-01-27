module TestParsers where
{-
import           Control.Applicative
import qualified Data.Text           as T
import           Enpassing.Music
import           Euterpea
import           Parsers
import           Test.Tasty
import           Test.Tasty.HUnit
import           Text.Parsec
import           Text.Parsec.Text

tests = testGroup "Parser Tests" [
  test_parse_note,
  test_parse_quality,
  test_parse_chord]
  --testProperty "Chord parsing test:" chord_parse_test]

test_parse_note = testGroup "Note Parsing Tests" $ zipWith (parse_test parse_note note_eq) note_names note_pitches
  where
    note_names   = ["C","C#","Db","D","D#","Eb","E","F","F#","Gb","G","G#","Ab","A","A#","Bb","B"]
    note_pitches =  [C,  Cs,  Df,  D,  Ds,  Ef,  E,  F,  Fs,  Gf,  G,  Gs,  Af,  A,  As,  Bf,  B]
    note_eq n1 n2 = pcToInt n1 == pcToInt n2

test_parse_quality = testGroup "Quality Parsing Tests" $ uncurry (parse_test parse_mode (==)) <$> qualities
  where
    f b a = (a, b)
    qualities = (f Major <$> ["M","Maj","maj"]) -- test the triangle
             ++ (f Minor <$> ["m", "Min", "min", "-"])
             ++ (f Mixolydian <$> ["dom", ""])
             ++ (f (CustomMode "Aug") <$> ["aug", "+"])
             ++ (f (CustomMode "Dim") <$> ["dim", "o"])

test_parse_chord = testGroup "Chord Parsing Tests" $ uncurry (parse_test parse_chord (==)) <$> chords
  where
    chords = [("Cmaj", Chord C Major []), ("Cmaj7", Chord C Major [Add 7]),
      ("AM9", Chord A Major [Add 7, Add 9])]

parse_test ::(Eq a, Show a) => Parser a -> (a -> a -> Bool) -> String -> a -> TestTree
parse_test parser p str o = testCase (str ++" should parse to "++ show o) $ case parse parser "" (T.pack str) of
  Left err -> assertFailure $ show err
  Right x  -> assertBool (str++" should parse to "++ show o ++", actually parsed "++ show x) (p o x)

--chord_parse_test :: Chord -> Bool
--chord_parse_test chord = runParser parse_chord (show chord) == pure chord
-}
