module TestScale where

import Test.QuickCheck

import Music.Theory
import Test.Music.Theory.Pitch

[c4,cs4,d4,ds4,e4,f4,fs4,g4,gs4,a4,as4,b4,c5,cs5,d5,ds5,e5,f5,fs5,g5,gs5,a5,as5,b5] = map (\x -> trans x (C, 4)) [0..23]

--major chords
c_major = [c4, d4, e4, f4, g4, a4, b4]
g_major = [g4, a4, b4, c5, d5, e5, fs5]
b_flat_major = [as4, c5, d5, ds5, f5, g5, a5]

--minor chords
c_minor = [c4, d4, ds4, f4, g4, gs4, as4]
a_minor = [a4, b4, c5, d5, e5, f5, g5]
f_minor = [f4, g4, gs4, as4, c5, cs5, ds5]

tests = testGroup "Testing that scales are reproduced faithfully" [major_tests, minor_tests, degree_tests]

major_tests = testGroup "Major Scales" [scale_test c4 Major c_major,
                                        scale_test g4 Major g_major,
                                        scale_test as4 Major b_flat_major]

minor_tests = testGroup "Minor Scales" [scale_test c4 Minor c_minor,
                                        scale_test a4 Minor a_minor,
                                        scale_test f4 Minor f_minor]

scale_test :: Pitch -> Mode -> Scale -> TestTree
scale_test pitch mode scale = testCase message $ assertEqual "" scale new_scale
  where new_scale = take 7 $ infinite_scale (pitch, mode)
        message = "Testing that the scale "++ show scale ++" can be produced from"++ show (pitch, mode)

degree_tests = let cases = [((C, Major), G),
                            ((C, Minor), Gs)]               in testGroup "Degree Tests" $ map (uncurry scale_degree_test) cases


scale_degree_test :: Key -> PitchClass -> TestTree
scale_degree_test key note = testCase "" $ assertBool "fail" . isJust $ scale_degree key note

-- If a note is in a scale, the scale degree of that note should be the same


{- forall scale, a in scale,
for some scale s
for some key k
for some n in [0..6]:
  scale_degree k

  where

  scale_degree :: Key -> PitchClass -> Maybe ScaleDegree
  -}

-}
