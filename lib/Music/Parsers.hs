{-# LANGUAGE LambdaCase, MultiWayIf #-}
module Music.Theory.Parsers where

--
--import           Control.Monad
--import           Data.Maybe             (maybeToList, fromMaybe)
--import           Data.Ratio
--import qualified Data.Text              as T
--import qualified Data.Text.IO           as T (readFile)
--import           Data.Attoparsec.Combinator (lookAhead)
--import           Data.Attoparsec.Text       hiding (D, I)

--import Data.Functor
--import Control.Lens
--import Control.Applicative
--import Data.List
import Data.Char
import Text.ParserCombinators.ReadP

import Music.Theory

readInt :: ReadP Int
readInt = munch1 isDigit

readNote :: ReadP Note
readNote = char 'A' $> A
       <|> char 'B' $> B
       <|> char 'C' $> C
       <|> char 'D' $> D
       <|> char 'E' $> E
       <|> char 'F' $> F
       <|> char 'G' $> G

readPitchClass :: ReadP PitchClass
readPitchClass = do
  note <- readNote
  acc <- option natural $ choice
  [ char '#' $> sharp
  , char 'b' $> flat ]
  pure (acc note)

readAccidental :: ReadP Accidental
readAccidental = char '#' $> Sharp
             <|> char 'b' $> Flat
             <|> string "" $> Natural

-- Taken from https://rosettacode.org/wiki/Roman_numerals/Decode#Fold
readDegree :: ReadP Degree
readDegree = do
      numerals <- romanParser
      either (fail . show) pure (matching mkDegree (partial + last))
      where
        (partial, last) = foldl' acc (0, 0) numerals

        romanParser :: Parser [Int]
        romanParser =
          many' . choice $
            zipWith
            (\c n -> (satisy ((c==) . toUpper) $> n)
            "IVXLCDM"
            [1, 5, 10, 50, 100, 500, 1000]

        acc :: (Int, Int) -> Int -> (Int, Int)
        acc (partial, old) new =
        if new <= old
          then (partial + old, new)
          else (partial - old, new)

instance Read Degree where
  readsPrec _ = readR_to_S readDegree

instance Show Degree where
  show (Degree n) =
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

readInterval :: ReadP Interval
readInterval = do
  d <- read @Int <$> munch1 isDigit
  q <- char 'M' $> Major
   <|> char 'P' $> Perfect
   <|> char 'm' $> Minor
   <|> char 'd' $> Diminished
   <|> char 'A' $> Augmented
  pure (newInterval q d)

instance Read Interval where
  readsPrec _ = readR_to_S readInterval

instance IsString Interval where
  fromString = read

instance Show Interval where
  show (Interval quality degree) =
    let
      qualStr = case quality of
        Major -> if isPerfect degree then "Perfect" else "Major"
        _     -> show quality
    in
      qualStr <> " " <> show degree

-- Chord Quality
readChordQuality :: ReadP ChordQuality
readChordQuality =
      (string "Maj" <|> string "maj") $> MajorChord
  <|> (string "Min" <|> string "min") $> MinorChord
  <|> (string "M"   <|> string "Δ") $> MajorChord
  <|> (string "m"   <|> string "-") $> MinorChord
  <|> (string "+"   <|> string "aug") $> AugmentedChord
  <|> (string "o"   <|> string "dim") $> DiminishedChord
  <|> string "sus2" $> Sus2Chord
  <|> string "sus4" $> Sus4Chord
  <|> string "" $> DominantChord

instance Read ChordQuality where
  readsPrec _ readP_to_S readChordQuality

instance Show ChordQuality where
  show = \case
    MinorChord      -> "m"
    DominantChord   -> ""
    MajorChord      -> "maj"
    AugmentedChord  -> "aug"
    DiminishedChord -> "dim"
    Sus2Chord       -> "sus2"
    Sus4Chord       -> "sus4"
    _               -> error "Mode Parse Error"

-- Extensions
-- 6, 7, 9, 11, 13
readExtension :: ReadP Interval
readExtension = do
  acc <- readAccidental

--instance Parseable Extension where
--  parser = choice
--    [ Acc <$> parserAccidental parserDegreeNum
--    , "no" $> No <*> parserDegreeNum
--    , "sus2" $> Sus2
--    , "sus4" $> Sus4 ]
--
--  unparse = \case
--    Acc acc -> unparseAccidental unparseDegreeNum acc
--    Sus2    -> "sus2"
--    Sus4    -> "sus4"
--    No deg  -> "no" <> unparseDegreeNum deg
--
--parseTailExtension :: Parser Extension
--parseTailExtension = char '(' *> parser <* char ')'
--
--parseExtensions :: Parser [Extension]
--parseExtensions = do
--  head <- optional parser
--  tail <- many' parseTailExtension
--  pure $ maybe tail (:tail) head
--
--
----Chord Parsing
--fromEitherParser :: Show e => Either e a -> Parser a
--fromEitherParser = either (fail . show) pure
--
--chordEnd :: Parser ()
--chordEnd = do
--  next <- peekChar
--  if maybe True (\c -> isSpace c || c == '|') next
--    then pure ()
--    else fail ("Chord End Failed, found chord " <> show next)
--
--unparseExtensions :: ExtendedClass c => c -> String
--unparseExtensions chord =
--  let bracket x = "(" <>unparse x<> ")"
--  in case sort (chord^.exts) of
--    []                       -> ""
--    e@(Acc (Natural _)) : es -> unparse e <> foldMap bracket es
--    _                        -> foldMap bracket (chord^.exts)
--
--
--instance Parseable ExtendedChord where
--  parser = do
--    root <- parser
--    mode <- parser
--    exts <- parseExtensions
--    fromEitherParser $ extended root mode exts
--
--  unparse chord = join
--    [ chord^.root.to unparse
--    , chord^.mode.to unparse
--    , unparseExtensions chord ]
--
--instance Parseable InterpretedChord where
--  parser = do
--    accDegree <- parserAccidental parser
--    let deg = over _Accidental (^._LetterCase) accDegree
--    let accMode = accDegree^._Accidental.to asMode
--    simpleMode <- option Nothing (Just <$> parseSimpleMode)
--    let mode = fromMaybe accMode simpleMode
--    exts <- parseExtensions
--    fromEitherParser $ interpreted deg mode exts
--    where
--      asMode :: LetterCase a -> Mode
--      asMode = \case
--        Upper _ -> Ionian
--        Lower _ -> Aeolian
--
--      parseSimpleMode :: Parser Mode
--      parseSimpleMode = char '+' $> Augmented
--                    <|> char 'o' $> Diminished
--
--  unparse chord = join
--    [ showMode
--    , unparseExtensions chord ]
--    where
--      upper = chord^.degree.to unparse
--      lower = toLower <$> upper
--
--      showMode = case chord^.mode of
--        Ionian     -> upper
--        Aeolian    -> lower
--        Mixolydian -> upper
--        Augmented  -> upper <> "+"
--        Diminished -> lower <> "o"
--
--
--instance Parseable SlashChord where
--  parser = do
--    chord <- parser
--    skipSpace *> char '/' <* skipSpace -- parse slash
--    root <- parser <* chordEnd
--    fromEitherParser $ slash chord root
--    where parseSlash = skipSpace *> char '/' <* skipSpace
--
--  unparse slashChord =
--    unparse (slashChord^.chord) <> "/" <> unparse (slashChord^.bass)
--
--
--instance Parseable Chord where
--  parser = Slash       <$> parser
--       <|> Extended    <$> parser
--       <|> Interpreted <$> parser
--
--  unparse = \case
--    Extended x -> unparse x
--    Interpreted x -> unparse x
--    Slash x -> unparse x
