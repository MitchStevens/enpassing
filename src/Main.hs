module Main where

import           Control.Exception
import           Data.Either
import           Data.Functor.Compose
import           Data.Text.IO                   as T
import           Enpassing.Changes.Passing
import           Enpassing.Changes.Substitution
import           Enpassing.Music
import           Enpassing.Playable
import           Euterpea                       hiding (play)
import           Parsers
import           System.IO.Unsafe               (unsafePerformIO)
import           Test.QuickCheck.Gen
import           Text.Parsec
import           UnliftIO.Exception

io_sheet :: IO Sheet
io_sheet = fromEitherM $ parse parse_sheet "" <$> T.readFile "res/but_not_for_me.txt" --"res/over_the_rainbow.txt"

main :: IO ()
main = with_subs
  where
    no_subs   = io_sheet >>= print
    with_subs = io_sheet >>= generate_substitutions >>= print
    --with_additions = io_sheet >>= generate_passing_chords >>= print_and_play

print_and_play sheet = print sheet >> play_drum sheet

instance Exception ParseError where
  toException = SomeException
  fromException = undefined
  displayException = show
