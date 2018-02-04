module Parsers where

import           Control.Monad          (void)
import           Data.Char              (digitToInt, isAlpha, isDigit)
import           Data.Map.Strict        (findWithDefault, fromList)
import           Data.Maybe             (maybeToList)
import           Data.Ratio
import qualified Data.Text              as T
import qualified Data.Text.IO           as T (readFile)
import           Enpassing.Music
import           Euterpea
import           Text.Parsec
import           Text.Parsec.Combinator
import           Text.Parsec.Text

parse_note :: Parser PitchClass
parse_note = do
  note <- read . pure <$> oneOf "ABCDEFG"
  accidental <- option 0 parse_accidental
  return $ notes !! ((pcToInt note + accidental) `mod` 12)
    where notes = [C, Cs, D, Ds, E, F, Fs, G, Gs, A, As, B]

parse_accidental :: Parser Int
parse_accidental = (char '#' >> return 1)
               <|> (char 'b' >> return (-1))
               <|> (char '♮' >> return 0)

parse_mode :: Parser Mode
parse_mode = option Mixolydian $ choice [majP, minP, domP, augP, dimP]
  where
    majP = (try (string "Maj")
        <|> try (string "maj")
        <|> try (string "M"   <* end)
        <|> try (string "Δ")) >> return Major
    minP = (try (string "m"   <* end)
        <|> try (string "min")
        <|> try (string "Min")
        <|> try (string "-"   <* end)) >> return Minor
    domP =  try (string "dom" <* end)  >> return Mixolydian
    augP = (string "+" <|> string "aug") >> return (CustomMode "Aug")
    dimP = (string "o" <|> string "dim") >> return (CustomMode "Dim")
    end = lookAhead (try (void $ satisfy (not.isAlpha)) <|> eof)

parse_natural :: Parser Int
parse_natural = read <$> many (satisfy isDigit)

parse_extension :: Parser Extension
parse_extension = do
  char '('
  a <- option Add ext
  n <- parse_natural
  char ')'
  return $ a n
    where
      ext = (string "#"   >> return Sharp)
        <|> (string "b"   >> return Flat)
        <|> (string "add" >> return Add)

parse_chord :: Parser Chord
parse_chord = do
  root     <- parse_note
  quality  <- parse_mode
  qual_num <- optionMaybe parse_qual_num
  exts     <- many parse_extension
  let all_exts = maybeToList qual_num ++ exts
  case mk_chord root quality all_exts of
    Left err    -> parserFail (show err)
    Right chord -> return chord

parse_qual_num :: Parser Extension
parse_qual_num = (char '7'          >> return (Add 7))
             <|> (char '9'          >> return (Add 9))
             <|> (try (string "11") >> return (Add 11))
             <|> (try (string "13") >> return (Add 13))

split_bars :: Parser a -> Parser [a]
split_bars p = b *> sepBy p b
  where b = char '|'

parse_bar :: Parser Bar
parse_bar = do
  chords <- pad *> endBy parse_chord pad
  let duration = recip . toRational $ length chords
  return $ if null chords
    then [Rest 1]
    else map (Note duration) chords
    where pad = many $ oneOf " "

parse_line :: Parser [Bar]
parse_line = char '|' *> manyTill (parse_bar <* char '|') (void newline <|> eof)

parse_sheet :: Parser Sheet
parse_sheet = do
  name <- manyTill anyChar newline
  key  <- string "Key: " *> parse_note <* newline
  bars <- many parse_line
  return $ Sheet name (key, Major) $ (concat.concat) bars

