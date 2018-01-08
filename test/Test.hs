import Test.Tasty
import Control.Monad
import TestParsers
import TestChord
import TestScale
import TestSheet

main = all_tests >>= defaultMain
  where all_tests = (\tree -> testGroup "" [non_io_tests, tree]) <$> io_tests

non_io_tests :: TestTree
non_io_tests = testGroup "Non IO Tests" [
  TestParsers.tests, TestChord.tests, TestScale.tests]

io_tests :: IO TestTree
io_tests = testGroup "IO Tests" <$> sequence [TestSheet.tests]