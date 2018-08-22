module Enpassing.Transition.Predicate where

import           Control.Comonad
import           Control.Lens
import           Data.Maybe
import           Enpassing.Theory
import           Enpassing.Transition.MusicM
import           Prelude                     hiding (not, (&&), (||))


-- Substution Predicates

{-| check the  |-}
hasRoot :: PitchClass -> MusicM Chord -> Bool
hasRoot root' mChord = case snd mChord of
    Extended    c -> root' == c ^. root
    Interpreted _ -> False
    Slash       c -> root' == c ^. root

{-| The Keyed |-}
hasMode :: Mode -> MusicM Chord -> Bool
hasMode mode' mchord = mode' == (mchord ^. _2 . mode)

{-| |-}
hasDegree :: Degree -> MusicM Chord -> Bool
hasDegree deg' mchord = maybe False (deg' ==) mdeg
  where mdeg = mchord ^? _2 . _Interpreted . degree

{-| |-}
hasExtension :: Extension -> MusicM Chord -> Bool
hasExtension e mchord = e `elem` (mchord ^. _2 . exts)

