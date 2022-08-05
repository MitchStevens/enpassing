module Music.Theory.Semitones where

import Data.Function (on)

mod12 :: Integral n => n -> n
mod12 x = x `mod` 12

div12 :: Integral n => n -> n
div12 x = x `div` 12

class Semitones t where
  steps :: t -> Int

instance Semitones () where
  steps _ = 0

instance Semitones Int where
  steps = id

equiv :: Semitones t => t -> t -> Bool
equiv = (==) `on` steps

{-
  x `diff` y - y `diff` x == 0
-}
diff :: Semitones t => t -> t -> Int
x `diff` y = steps x - steps y

getOctave :: Semitones a => a -> Int
getOctave = div12 . steps

getIntegerPitchClass :: Semitones a => a -> Int
getIntegerPitchClass = mod12 . steps

octaveEq :: Semitones t => t -> t -> Bool
x `octaveEq` y = mod12 (x `diff` y) == 0
