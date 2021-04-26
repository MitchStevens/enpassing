module Demo.Progression where

import Control.Concurrent

import Music.Theory hiding ((/))


type Duration = Rational
data Player = Player { bpm :: Int }

class Playable a where
  play :: Player -> (Duration, a) -> IO ()

instance Playable Chord where
  play (Player bpm) (duration, chord) = do
    threadDelay durationInMicroseconds
    putStrLn ("played chord" <> show chord)
      where durationInMicroseconds = round (1000000 * duration * 60 * 4 / fromIntegral bpm)

data Progression a = Progression { curr :: (Duration, a), next :: [(Duration, a)] } deriving (Show)

fromList :: [(Duration, a)] -> Progression a
fromList l = Progression (head l) (tail l)

playCurr :: Playable a => Player -> Progression a -> IO ()
playCurr player progression = play player (curr progression)

advance :: Progression a -> Progression a
advance (Progression x []) = Progression x []
advance (Progression x (y:ys)) = Progression y (ys <> [x])

regress :: Progression a -> Progression a
regress p@(Progression curr next) = case next of
  [] -> p
  _ -> Progression (last next) (curr : init next)

substitute :: (a -> a) -> Progression a -> Progression a
substitute f (Progression (duration, chord) next) = Progression (duration, f chord) next

passing :: (a -> a) -> Progression a -> Progression a
passing f (Progression (duration, chord) next) = Progression (d, chord) $ (d, f chord) : next
  where d = duration / 2

remove :: Progression a -> Progression a
remove progression = case regress progression of
  Progression _ [] -> progression
  Progression (d1, c1) ((d2, c2):rest) -> advance $ Progression (d1+d2, c1) rest
