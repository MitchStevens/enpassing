{-# LANGUAGE OverloadedStrings #-}

import Enpassing.Music
import Enpassing.Changes
import Text.Printf

data InteractiveState = InteractiveState
  { sheet :: Maybe Sheet
  , style :: Style
  }

interactive_state = InteractiveState Nothing None

interactive_mode :: IO ()
interactive_mode = undefined

set_sheet :: FilePath -> InteractiveState -> IO InteractiveState
set_sheet path state = do
  file <- readFile path
  case parse parse_sheet "" file of
    Left error  -> printf "Couldn't read file '%s': parse error %s" (path) (show error)
    Right sheet -> printf "Set new sheet '%s'" (name sheet)

set_style 

cli_stylise :: InteractiveState -> [Flag] -> IO ()
cli_stylise args = do


  
read_sheet :: T.Text -> IO (Maybe Sheet)
read_sheet sheet_location = do
  sheet_text <- T.readFile sheet_location
  let sheet = parse parse_sheet "" sheet_text
  return $ either (const Nothing) Just sheet
  
cli_error :: IO ()
cli_error = putStrLn "Poorly formed instruction, use \'--help\' to print help"
  
cli_help :: IO ()
cli_help = readFile "res/help.txt" >>= putStrLn

print_and_play :: Sheet -> IO ()
print_and_play sheet = print sheet >> play_drum sheet