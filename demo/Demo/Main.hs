module Main where

import Control.Concurrent
import Control.Monad
import Control.Concurrent.MVar
import Demo.Progression as Progression

import Music.Theory

twoFiveOne, fourChords :: Progression Chord
twoFiveOne = Progression.fromList [(1, d Minor), (1, g Major), (2, c Major)]
fourChords = Progression.fromList [(1, c Major), (1, g Major), (1, a Minor), (1, f Major)]

defaultPlayer = Player 120

playDemoLoop :: (Show p, Playable p) => MVar (Progression p) -> IO ()
playDemoLoop mvar = forever $ do
  progression <- takeMVar mvar
  print progression
  putMVar mvar (advance progression)
  playCurr defaultPlayer progression

userInteraction :: MVar (Progression Chord) -> IO ()
userInteraction mvar = forever $ do
  line <- getLine
  case line of
    "251" -> putMVar mvar twoFiveOne
    "4 chords" -> putMVar mvar fourChords
    _ -> putStrLn "dunno lol"


main :: IO ()
main = do
  mvar <- newMVar twoFiveOne
  forkIO $ playDemoLoop mvar
  forkIO $ userInteraction mvar
  threadDelay (1000000 * 50)
  putStrLn "hello"
  pure ()
