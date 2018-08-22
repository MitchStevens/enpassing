module Enpassing.Theory.Sheet where

import           Enpassing.Theory.Chord
import           Enpassing.Theory.Types

data Sheet = Sheet
  { name     :: String
  , sheetKey :: Key
  --, timeSignature :: Key
  , chords   :: [Chord] }
