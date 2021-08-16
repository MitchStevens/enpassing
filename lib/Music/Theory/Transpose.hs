module Music.Theory.Transpose where

import Control.Lens
import Music.Theory.Interval
import Music.Theory.Semitones

class Semitones t => Transpose t where
  shift :: Interval -> t -> t

instance Transpose Int where
  shift = (+) . steps

transpose :: Transpose t => Int -> t -> t
transpose = shift . stepsToInterval

-- instance Transpose () where
--   shift _ () = ()
--
-- instance Transpose Int where
--   shift n t = steps n + t


--shiftOctave :: (Transpose t) => Int -> t -> t
--shiftOctave n t = shift ("p8")

-- (octave .~ n) a `octaveEq` a

{-
  Is `octave` a valid lens?
    getOctave (setOctave s b) = b -- holds assuming transposition property
    setOctave (getOctave s) s = s -- holds
    setOctave (setOctave s c) b = setOctave s b
      -- holds assuming trans. prop. and `trans n . trans m == trans (m+n)`
-}
octave :: (Semitones t, Transpose t) => Lens' t Int
octave = lens getOctave setOctave
  where
    getOctave = div12 . steps

    setOctave :: (Semitones t, Transpose t) => t -> Int -> t
    setOctave s b = transpose (12 * b - steps s) s


octaveEq :: Semitones t => t -> t -> Bool
x `octaveEq` y = mod12 (steps x - steps y) == 0

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
