module Enpassing.Parsers where

import           Enpassing.Theory

import           Control.Monad
import           Data.Maybe             (maybeToList, fromMaybe)
import           Data.Ratio
import qualified Data.Text              as T
import qualified Data.Text.IO           as T (readFile)
import           Data.Attoparsec.Combinator (lookAhead)
import           Data.Attoparsec.Text       hiding (D, I)
import           Data.Char                  (isAlpha, isLower, isSpace, isUpper,
                                             toLower, toUpper)
import Data.Functor
import Control.Lens
import Control.Applicative
import Data.List

class Parseable a where
  parser :: Parser a
  unparse :: a -> String


instance Parseable Note where
  parser = choice
    [ char 'A' $> A
    , char 'B' $> B
    , char 'C' $> C
    , char 'D' $> D
    , char 'E' $> E
    , char 'F' $> F
    , char 'G' $> G ]

  unparse = show


instance Parseable PitchClass where
  parser = do
    note <- parser
    acc <- option natural $ choice
      [ char '#' $> sharp
      , char 'b' $> flat ]
    pure (acc note)

  unparse p = case p^._Wrapped of
    Flat    n -> unparse n <> "b"
    Natural n -> unparse n
    Sharp   n -> unparse n <> "#"


parserAccidental :: Parser a -> Parser (Accidental a)
parserAccidental parser = choice
  [ char '#' $> Sharp   <*> parser
  , char 'b' $> Flat    <*> parser
  , ("add" <|> "") $> Natural <*> parser]

unparseAccidental :: (a -> String) -> (Accidental a -> String)
unparseAccidental unparse = \case
  Flat x    -> "b" <> unparse x
  Natural x -> unparse x
  Sharp x   -> "#" <> unparse x


-- Taken from https://rosettacode.org/wiki/Roman_numerals/Decode#Fold
instance Parseable Degree where
  parser =
    let
      romanParser :: Parser [Int]
      romanParser = many' . choice $ zipWith
        (\c n -> (char (toLower c) <|> char c) $> n)
        "IVXLCDM"
        [1, 5, 10, 50, 100, 500, 1000]

      acc :: (Int, Int) -> Int -> (Int, Int)
      acc (partial, old) new =
        if new <= old
          then (partial + old, new)
          else (partial - old, new)
    in do
      numerals <- romanParser
      let (partial, last) = foldl' acc (0, 0) numerals
      either (fail . show) pure (matching mkDegree (partial + last))

  unparse (Degree n) =
    let
      f :: Char -> Char -> Char -> Int -> String
      f x v i n = case mod n 10 of
        0 -> ""
        1 -> [i]
        2 -> [i,i]
        3 -> [i,i,i]
        4 -> [i,v]
        5 -> [v]
        6 -> [v,i]
        7 -> [v,i,i]
        8 -> [v,i,i,i]
        9 -> [i,x]

      ones      = div n 1
      tens      = div n 10
      hundreds  = div n 100
      thousands = div n 1000
    in mconcat
      [ replicate thousands 'M'
      , f 'M' 'D' 'C' hundreds
      , f 'C' 'L' 'X' tens
      , f 'X' 'V' 'I' ones ]


--Should DegreeNum be a wrapper?
parserDegreeNum :: Parser Degree
parserDegreeNum = do
  n <- decimal
  maybe (fail "DegreeNum parse ") pure (n ^? mkDegree)

unparseDegreeNum :: Degree -> String
unparseDegreeNum (Degree d) = show d

instance Parseable a => Parseable (LetterCase a) where
  parser = do
    (text, a) <- match parser
    if | T.all isUpper text -> pure (Upper a)
       | T.all isLower text -> pure (Lower a)
       | otherwise          -> fail "couldn't parse lettercase"

  unparse = \case
    Upper x -> map toUpper (unparse x)
    Lower x -> map toLower (unparse x)

instance Parseable Mode where
  parser = option Mixolydian $ choice
    [ ("Maj" <|> "maj") $> Ionian
    , ("Min" <|> "min") $> Aeolian
    , ("M" <|> "Δ") $> Ionian
    , ("m" <|> "-") $> Aeolian
    , "dom" $> Mixolydian
    , ("+" <|> "aug") $> Augmented
    , ("o" <|> "dim") $> Diminished ]

  unparse = \case
    Aeolian    -> "min"
    Ionian     -> "maj"
    Mixolydian -> ""
    Augmented  -> "+"
    Diminished -> "o"
    _          -> error "Mode Parse Error"

instance Parseable Extension where
  parser = choice
    [ Acc <$> parserAccidental parserDegreeNum
    , "no" $> No <*> parserDegreeNum
    , "sus2" $> Sus2
    , "sus4" $> Sus4 ]

  unparse = \case
    Acc acc -> unparseAccidental unparseDegreeNum acc
    Sus2    -> "sus2"
    Sus4    -> "sus4"
    No deg  -> "no" <> unparseDegreeNum deg

parseTailExtension :: Parser Extension
parseTailExtension = char '(' *> parser <* char ')'

parseExtensions :: Parser [Extension]
parseExtensions = do
  head <- optional parser
  tail <- many' parseTailExtension
  pure $ maybe tail (:tail) head


--Chord Parsing
fromEitherParser :: Show e => Either e a -> Parser a
fromEitherParser = either (fail . show) pure

chordEnd :: Parser ()
chordEnd = do
  next <- peekChar
  if maybe True (\c -> isSpace c || c == '|') next
    then pure ()
    else fail ("Chord End Failed, found chord " <> show next)

unparseExtensions :: ExtendedClass c => c -> String
unparseExtensions chord =
  let bracket x = "(" <>unparse x<> ")"
  in case sort (chord^.exts) of
    []                       -> ""
    e@(Acc (Natural _)) : es -> unparse e <> foldMap bracket es
    _                        -> foldMap bracket (chord^.exts)


instance Parseable ExtendedChord where
  parser = do
    root <- parser
    mode <- parser
    exts <- parseExtensions
    fromEitherParser $ extended root mode exts

  unparse chord = join
    [ chord^.root.to unparse
    , chord^.mode.to unparse
    , unparseExtensions chord ]

instance Parseable InterpretedChord where
  parser = do
    accDegree <- parserAccidental parser
    let deg = over _Accidental (^._LetterCase) accDegree
    let accMode = accDegree^._Accidental.to asMode
    simpleMode <- option Nothing (Just <$> parseSimpleMode)
    let mode = fromMaybe accMode simpleMode
    exts <- parseExtensions
    fromEitherParser $ interpreted deg mode exts
    where
      asMode :: LetterCase a -> Mode
      asMode = \case
        Upper _ -> Ionian
        Lower _ -> Aeolian

      parseSimpleMode :: Parser Mode
      parseSimpleMode = char '+' $> Augmented
                    <|> char 'o' $> Diminished

  unparse chord = join
    [ showMode
    , unparseExtensions chord ]
    where
      upper = chord^.degree.to unparse
      lower = toLower <$> upper

      showMode = case chord^.mode of
        Ionian     -> upper
        Aeolian    -> lower
        Mixolydian -> upper
        Augmented  -> upper <> "+"
        Diminished -> lower <> "o"


instance Parseable SlashChord where
  parser = do
    chord <- parser
    skipSpace *> char '/' <* skipSpace -- parse slash
    root <- parser <* chordEnd
    fromEitherParser $ slash chord root
    where parseSlash = skipSpace *> char '/' <* skipSpace

  unparse slashChord =
    unparse (slashChord^.chord) <> "/" <> unparse (slashChord^.bass)


instance Parseable Chord where
  parser = Slash       <$> parser
       <|> Extended    <$> parser
       <|> Interpreted <$> parser

  unparse = \case
    Extended x -> unparse x
    Interpreted x -> unparse x
    Slash x -> unparse x

{-
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
  return $ Sheet name (key, Major) $ (init.concat.concat) bars
-}
