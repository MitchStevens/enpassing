module Music.Theory.Interval
  ( Interval (..),
    intervalDegree,
    intervalQuality,
    stepsToInterval,
  )
where

import Control.Lens
import Data.Char
import Data.Function (on)
import Data.Maybe
import Data.String
import GHC.Generics
import Music.Theory.Accidental
import Music.Theory.Degree
import Music.Theory.Semitones
import Text.Read

data Interval = Interval
  { _intervalQuality :: Accidental,
    _intervalDegree :: Degree
  }
  deriving (Generic)

makeLenses ''Interval

instance Eq Interval where
  (==) = (==) `on` steps

instance Ord Interval where
  compare = compare `on` steps

instance Show Interval where
  show (Interval quality d@(Degree n)) = showQuality <> show (n + 1)
    where
      showQuality = case quality of
        DoubleFlat -> if not (isPerfect d) then "d" else show DoubleFlat
        Flat -> if isPerfect d then "d" else "m"
        Natural -> if isPerfect d then "P" else "M"
        Sharp -> "A"
        _ -> show quality

instance Semigroup Interval where
  i1@(Interval q1 d1) <> i2@(Interval q2 d2) = Interval (Offset acc) (d1 <> d2)
    where
      acc = steps i1 + steps i2 - steps (d1 <> d2)

instance Monoid Interval where
  mempty = Interval mempty mempty

instance IsString Interval where
  fromString s = fromMaybe (error ("error parsing interval " <> s)) $ do
    d <- mkDegree <$> readMaybe (tail s)
    q <- case head s of
      'M' -> pure Natural
      'P' -> pure Natural
      'm' -> pure Flat
      'd' -> pure (if isPerfect d then Flat else DoubleFlat)
      'A' -> pure Sharp
      _ -> Nothing
    pure (Interval q d)

instance Enum Interval where
  toEnum = stepsToInterval
  fromEnum = steps

instance Semitones Interval where
  steps (Interval quality degree) = steps degree + steps quality

instance HasAccidental Interval where
  accidental = intervalQuality

stepsToInterval :: Int -> Interval
stepsToInterval n = Interval quality degree'
  where
    deg = case mod12 n of
      0 -> "P1"
      1 -> "m2"
      2 -> "M2"
      3 -> "m3"
      4 -> "M3"
      5 -> "P4"
      6 -> "d5"
      7 -> "P5"
      8 -> "m6"
      9 -> "M6"
      10 -> "m7"
      11 -> "M7"
      _ -> error "impossible"
    Interval quality (Degree d) = deg
    degree' = Degree (7 * (div12 n) + d)
