{-# LANGUAGE OverloadedStrings #-}
module UserInterface where

import           Enpassing.Theory
import           Enpassing.Transition

import           Data.Attoparsec
import qualified Data.Text            as T
import qualified Data.Text.IO         as T
import           Text.Printf
{-
data InteractiveState = InteractiveState
  { sheet :: Maybe Sheet
  , style :: Style Chord
  }

interactiveState = InteractiveState Nothing undefined

interactiveMode :: IO ()
interactiveMode = undefined

setSheet :: FilePath -> InteractiveState -> IO InteractiveState
setSheet path state = do
  file <- readFile path
  case parse parseSheet "" file of
    Left error  -> printf "Couldn't read file '%s': parse error %s" (path) (show error)
    Right sheet -> printf "Set new sheet '%s'" (name sheet)
{-
cliStylise :: InteractiveState -> [Flag] -> IO ()
cliStylise args = undefined
-}


readSheet :: T.Text -> IO (Maybe Sheet)
readSheet sheetLocation = do
  sheetText <- T.readFile sheetLocation
  let sheet = parse parse_sheet "" sheetText
  return $ either (const Nothing) Just sheet

cliError :: IO ()
cliError = putStrLn "Poorly formed instruction, use \'--help\' to print help"

cliHelp :: IO ()
cliHelp = readFile "res/help.txt" >>= putStrLn

{-
print_and_play :: Sheet -> IO ()
print_and_play sheet = print sheet >> play_drum sheet
-}
-}