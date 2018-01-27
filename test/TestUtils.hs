module TestUtils (
  same_elements
) where

import           Data.List ((\\))

same_elements :: Eq a => [a] -> [a] -> Bool
same_elements x y = null (x \\ y) && null (y \\ x)
