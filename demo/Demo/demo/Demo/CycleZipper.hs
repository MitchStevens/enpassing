module Demo.CycleZipper where

data CycleZipper a = CZ { left :: [a], curr :: a, right :: [a] }

fromList :: a -> [a] -> CZ a
fromList curr rest = CZ (drop n )
  where n = length rest `div` 2


cz :: [a] -> a -> [a] -> CZ a
cz =

next :: CycleZipper a -> CycleZipper a
next (CZ l v (r:rs)) = cz (v:l) r rs

previous :: CycleZipper a ->
previous
