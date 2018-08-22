import qualified Test.Enpassing.Theory.Degree as Degree
import qualified Test.Enpassing.Theory.Pitch  as Pitch
import qualified Test.Enpassing.Parsers                 as Parsers


main :: IO ()
main = do
  putStr "\n\n\n"
  test "Testing Degree" Degree.runTests
  test "Testing Parser" Parsers.runTests
  test "Testing Pitch"  Pitch.runTests

test :: String -> IO () -> IO ()
test name tests = putStrLn name *> tests *> putChar '\n'
