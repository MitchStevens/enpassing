module Main where

import Euterpea hiding (play)
import Enpassing.Music
import Enpassing.Changes.Substitution
import Parsers
import Text.Parsec
import Data.Text.IO as T
import Data.Either
import Enpassing.Playable
import Control.Exception
import UnliftIO.Exception
import Test.QuickCheck.Gen
import System.IO.Unsafe (unsafePerformIO)

io_sheet :: IO Sheet
io_sheet = fromEitherM $ parse parse_sheet "" <$> T.readFile "res/fly_me_to_the_moon.txt" --"res/over_the_rainbow.txt"

main :: IO ()
main = no_subs
  where
    no_subs   = io_sheet >>= print
    --with_subs = io_sheet >>= generate :: Sheet

print_and_play sheet = print sheet >> play_drum sheet

instance Exception ParseError where
  toException = SomeException
  fromException = undefined
  displayException = show