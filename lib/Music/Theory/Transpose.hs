module Music.Theory.Transpose where

import Control.Lens
import Music.Theory.Interval
import Music.Theory.Semitones

class Semitones t => Transpose t where
  shift :: Interval -> t -> t

instance {-# OVERLAPPING #-} Transpose () where
  shift _ () = ()

instance {-# OVERLAPPING #-} Transpose Int where
  shift = (+) . steps

transpose :: Transpose t => Int -> t -> t
transpose = shift . stepsToInterval

--shiftOctave :: (Transpose t) => Int -> t -> t
--shiftOctave n t = shift ("p8")

-- (octave .~ n) a `octaveEq` a

{-
  Is `octave` a valid lens?
    getOctave (setOctave s b) = b -- holds assuming transposition property
    setOctave (getOctave s) s = s -- holds
    setOctave (setOctave s c) b = setOctave s b
      -- holds assuming trans. prop. and `trans n . trans m == trans (m+n)`

  mod12 (steps (transpose (f s b) s)) = mod12 (steps s)
  mod12 (steps (transpose (f s b) s))
  mod12 (steps s + f s b)
  therefore mod12 (f s b) = 0, f s b = 12 * g s b

  div12 (steps (transpose (f s b) s)) = b
  div12 (steps s + 12 * g s b) = b
  div12 (steps s) + g s b = b
  g s b = b - div12 (steps s)
  f s b = (b - div12 (steps s)) * 12

-}
setOctave :: (Semitones t, Transpose t) => t -> Int -> t
setOctave s b = transpose (12 * (b - getOctave s)) s

octave :: (Semitones t, Transpose t) => Lens' t Int
octave = lens getOctave setOctave

--{-
--  0 < y `diff` (x `below` y) <= 12
--  (x `below` y) `sameOctave` x
---}
--below :: (Semitones t, Transpose t) => t -> t -> t
--x `below` y =
--  let x' = octave .~ (y^.octave) $ x
--  in
--    if steps x' < steps y
--       then x'
--       else (octave -~ 1) x'
--
--{-
--  0 < (x `above` y) `diff` y <= 12
--  y < (x `above` y)
---}
--above :: (Semitones t, Transpose t) => t -> t -> t
--x `above` y =
--  let x' = octave .~ (y^.octave) $ x
--  in
--    if steps x' > steps y
--       then x'
--       else (octave +~ 1) x'
