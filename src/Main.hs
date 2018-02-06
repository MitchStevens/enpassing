{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Exception
import           Data.Either
import           Data.Functor.Compose
import           Data.Text.IO                   as T
import qualified Data.Text as T
import           Enpassing.Changes
import           Enpassing.Music
import           Enpassing.Playable
import           Euterpea                       hiding (play)
import           Parsers
import           System.IO.Unsafe               (unsafePerformIO)
import           Test.QuickCheck.Gen
import           Text.Parsec
import           UnliftIO.Exception
import System.Environment
import Prelude hiding (readFile, putStrLn)

version = "0.1.0.0" :: String

main :: IO ()
main = getArgs >>= run_cli

instance Exception ParseError where
  toException = SomeException
  fromException = undefined
  displayException = show
