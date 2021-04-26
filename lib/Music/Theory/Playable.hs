module Music.Theory.Playable where

class Playable p where
  play :: p -> IO ()
