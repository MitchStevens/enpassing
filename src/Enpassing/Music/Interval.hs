module Enpassing.Music.Interval where

import Data.Monoid
import Euterpea
--import Enpassing.Music

data Interval = Unison   | Minor2nd   | Major2nd | Minor3rd
              | Major3rd | Perfect4th | Tritone  | Perfect5th
              | Minor6th | Major6th   | Minor7th | Major7th deriving (Eq, Show, Enum)

to_interval :: Int -> Interval
to_interval = toEnum . (`mod`12)

adj_pitchclass ::  PitchClass -> Interval -> PitchClass
adj_pitchclass p interval = fst . pitch $ (fromEnum interval) + (pcToInt p)

instance Monoid Interval where
  mempty = Unison
  mappend in1 in2 = to_interval $ fromEnum in1 + fromEnum in2
