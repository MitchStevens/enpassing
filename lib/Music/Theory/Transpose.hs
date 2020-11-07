module Music.Theory.Transpose where

import Control.Lens

mod12 :: Integral n => n -> n
mod12 x = x `mod` 12

div12 :: Integral n => n -> n 
div12 x = x `div` 12

{-
 - steps (shift n t) = steps t + n
 - shift n (shift m t) = shift (m+n) t
 -}
class Semitones t where
  steps :: t -> Int

class Transpose t where
  shift :: Integral n => n -> t -> t

instance Semitones Int where
  steps = id

instance Transpose Int where
  shift n t = fromIntegral (toInteger n + toInteger t) 

-- (octave .~ n) a `octaveEq` a
octave :: (Semitones t, Transpose t) => Lens' t Int
octave = lens (div12 . steps) setOctave
  where
    setOctave :: (Semitones t, Transpose t) => t -> Int -> t
    setOctave s b = shift x s
      where x = 12 * (b - div12 (steps s))
      
    {-
      set :: t -> Int -> t
      set s b = div12 (steps s) + x = b
      x = b - div12 (steps s)
    -}

octaveEq :: Semitones t => t -> t -> Bool
x `octaveEq` y = mod12 (steps x - steps y) == 0

{-
  x `diff` y - y `diff` x == 0
-}
diff :: (Semitones t, Num n) => t -> t -> n
x `diff` y = fromInteger $ toInteger $ steps x - steps y

{-
  0 < y `diff` (x `below` y) <= 12
  (x `below` y) `sameOctave` x
-}
below :: (Semitones t, Transpose t) => t -> t -> t
x `below` y =
  let x' = octave .~ (y^.octave) $ x
  in
    if steps x' < steps y 
       then x'
       else (octave -~ 1) x'

{-
  0 < (x `above` y) `diff` y <= 12
  y < (x `above` y)
-}
above :: (Semitones t, Transpose t) => t -> t -> t
x `above` y = 
  let x' = octave .~ (y^.octave) $ x
  in
    if steps x' > steps y
       then x'
       else (octave +~ 1) x'

{- Flatten a note by one semitone -}
flatten :: (Transpose t) => t -> t
flatten = shift (-1)

{- Sharpen a note by one semitone -}
sharpen :: (Transpose t) => t -> t
sharpen = shift 1
