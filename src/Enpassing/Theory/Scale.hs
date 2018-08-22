{-# LANGUAGE RankNTypes            #-}
module Enpassing.Theory.Scale (
  scalePitches,
  toPitchClasses,
  getExtensionPitch,
  findDegree
) where

import           Enpassing.Theory.Accidental
import           Enpassing.Theory.Class
import           Enpassing.Theory.Degree
import           Enpassing.Theory.Extension
import           Enpassing.Theory.Mode
import           Enpassing.Theory.Pitch
import           Enpassing.Theory.Transpose
import           Enpassing.Theory.Types

import           Control.Lens
import           Control.Monad               (join)
import           Data.List                   (elemIndex)

{-
A scale is a set of notes, ordered by pitch
Operations of scales
  Querying if a note is in a scale
  get the note in the scale
  taking a subscale
  range of a scale
-}

instance RootClass Scale Pitch where
  root = rootS
instance ModeClass Scale where
  mode = modeS

{-
_scale :: Keyed k => Iso' (k Pitch) Scale
_scale = iso
  (\k -> Scale (extract k) (k^.to key._2)
  (\s -> keyed (key (s^._root.to pc) (s^._mode)) (s^._root))

-}

{- Return the scale as a list of notes. The array has n notes -}
scalePitches :: Scale -> Int -> [Pitch]
scalePitches scale n =
  let upOctave = fmap (12+)
      intervals = join $ iterate upOctave (scale^.mode.to getIntervals)
  in take n $ scanl (flip shift) (scale^.root) intervals

{-  -}
toPitchClasses :: Scale -> [PitchClass]
toPitchClasses scale =
  let r = scale^.root.to pc
      intervals = scale^.mode.to getIntervals
  in init $ scanl (flip shift) r intervals

{- This could be an Iso `Scale -> Iso' Pitch (Accidental Extension)`  -}
getExtensionPitch :: Scale -> Getter (Accidental Extension) Pitch
getExtensionPitch scale = _Accidental . degree . re (findDegree scale)

{-
findExtension :: Scale -> Iso' Pitch (Accidental Extension)
findExtension scale = iso to from
  where
    to :: Pitch -> Accidental Extension
    to

    from :: Accidental Extension -> Pitch
    from
-}

findDegree :: Scale -> Prism' Pitch Degree
findDegree scale = prism' destruct construct
  where
    destruct :: Degree -> Pitch
    destruct deg =
      let
        (octave, notes) = divMod (deg^.re mkDegree - 1) (scale^.mode.to numNotes)-- n >= 0
        halfSteps = sum . take notes $ getIntervals (scale^.mode)
      in shift (12*octave + halfSteps) (scale^.root)

    construct :: Pitch -> Maybe Degree
    construct (Pitch p n) = do
      index <- elemIndex p (toPitchClasses scale)
      index ^? mkDegree
{-
 Given some keyed pitch, return Just a scale degree if the note is in the key, If is is not in the key, Return Nothing
  Ex.
    Keyed (C, Major)  C  -> Just I
    Keyed (Bb, Minor) Db -> Just III
    Keyed (A, Major)  Eb -> Nothing -}
