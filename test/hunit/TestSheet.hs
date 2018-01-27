module TestSheet where
{-
import Euterpea
import Enpassing.Music
import Parsers
import Text.Parsec
import Data.Text.IO as T
import Text.Parsec.String
import Test.Tasty
import Test.Tasty.HUnit
import Control.Exception
import UnliftIO.Exception

tests :: IO TestTree
tests = song_tests 16 <$> fly_me_to_the_moon

fly_me_to_the_moon :: IO Sheet
fly_me_to_the_moon = fromEitherM $ parse parse_sheet "" <$> T.readFile "res/fly_me_to_the_moon.txt"

over_the_rainbow :: IO Sheet
over_the_rainbow = fromEitherM $ parse parse_sheet "" <$> T.readFile "res/over_the_rainbow.txt"

song_tests :: Int -> Sheet -> TestTree
song_tests num_bars (Sheet name key bars) = testGroup ("Song: "++ show name) [
    testCase "Has the correct number of bars" $ assertEqual "" num_bars (length bars),
    testGroup "Bars all have duration 1" $ map (bar_test bars) [0..(length bars - 1)]]

bar_test :: [Bar] -> Int -> TestTree
bar_test bars n = testCase message assertion
  where
    message = "Bar "++ show n ++" ("++ show (bars !! n) ++") should have duration 1"
    assertion = assertEqual "" (bar_duration (bars !! n)) 1

instance Exception ParseError where
  toException = SomeException
  fromException = undefined
  displayException = show
  -}
