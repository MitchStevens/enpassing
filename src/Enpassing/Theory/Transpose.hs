module Enpassing.Theory.Transpose where

--Positive mod 12
mod12 :: Int -> Int
mod12 x = mod (mod x 12 + 12) 12

class Transpose t where
  shift :: Int -> t -> t
  trans :: Int -> t -> t
  trans n = shift (mod12 n)

instance Transpose Int where
  shift n = (n+)
