module Music.Theory.Semitones where

mod12 :: Integral n => n -> n
mod12 x = x `mod` 12

div12 :: Integral n => n -> n
div12 x = x `div` 12

class Semitones t where
  steps :: t -> Int

instance Semitones () where
  steps () = 0

instance Semitones Int where
  steps = id

{-
  x `diff` y - y `diff` x == 0
-}
diff :: Semitones t => t -> t -> Int
x `diff` y = steps x - steps y
