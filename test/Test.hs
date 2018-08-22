module Test where
{-
import           Control.Monad
import           Data.Proxy
import           System.Environment
import qualified TestChord
--import           TestInterpreted
import qualified PropInterpreted
import qualified PropScale
import qualified TestInterpreted
import           TestParsers
import           TestScale
import qualified TestSheet

main = do
  TestChord.main
  TestInterpreted.main

  PropInterpreted.main
  PropScale.main


non_io_tests :: [TestTree]
non_io_tests = [
  TestParsers.tests,
  TestChord.tests,
  TestScale.tests,
  TestInterpreted.tests]

io_tests :: IO [TestTree]
io_tests = sequence [
  TestSheet.tests]
-}
