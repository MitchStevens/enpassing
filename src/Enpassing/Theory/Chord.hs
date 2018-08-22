{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
module Enpassing.Theory.Chord where

import           Enpassing.Theory.Accidental
import           Enpassing.Theory.Class
import           Enpassing.Theory.Degree
import           Enpassing.Theory.Extension
import           Enpassing.Theory.Key
import           Enpassing.Theory.Mode
import           Enpassing.Theory.Pitch
import           Enpassing.Theory.Types

import           Control.Lens
import           Control.Monad           (foldM)
import           Data.Aeson
import           Data.Function           (on)
import           Data.List               (delete, foldl', insert, sort)
import           Debug.Trace

chordDegrees :: ExtendedClass c => c -> [Accidental Degree]
chordDegrees chord = foldl' (flip acc) basicTones (chord^.exts)
  where
    basicTones = [Natural d1, Natural d3, Natural d5]

    acc :: Extension -> [Accidental Degree] -> [Accidental Degree]
    acc ext = case ext of
      Acc a -> insert a
      Sus2  -> insert (Natural d2) . delete (Natural d3)
      Sus4  -> insert (Natural d4) . delete (Natural d3)
      No n  -> delete (Natural n)


--Extended Chord
data ExtendedChord = ExtendedChord
  { _rootE :: PitchClass
  , _modeE :: Mode
  , _extsE :: [Extension] }
    deriving (Show)
makeLenses 'ExtendedChord

instance Eq ExtendedChord where
  (==) = (==) `on` chordDegrees
instance RootClass ExtendedChord PitchClass where
  root = rootE
instance ModeClass ExtendedChord where
  mode = modeE
instance ExtendedClass ExtendedChord where
  exts = extsE


--Interpreted Chord
data InterpretedChord = InterpretedChord
  { _degreeI :: Accidental Degree
  , _modeI   :: Mode
  , _extsI   :: [Extension] }
    deriving (Show)
makeLenses 'InterpretedChord

instance Eq InterpretedChord where
  (==) = (==) `on` chordDegrees
instance ModeClass InterpretedChord where
  mode = modeI
instance (Functor f, Contravariant f) => DegreeClass f InterpretedChord where
  degree = degreeI._Accidental
instance ExtendedClass InterpretedChord where
  exts = extsI


--Slash Chord
data SlashChord = SlashChord
  { _chord :: ExtendedChord
  , _bass  :: PitchClass }
    deriving (Eq, Show)
makeLenses 'SlashChord

instance RootClass SlashChord PitchClass where
  root = chord . rootE
instance ModeClass SlashChord where
  mode = chord . modeE
instance ExtendedClass SlashChord where
  exts = chord . extsE


--Chord
data Chord
  = Extended ExtendedChord
  | Interpreted InterpretedChord
  | Slash SlashChord
  deriving (Eq, Show)
instance ModeClass Chord where
  mode = lens get set
    where
      get = \case
        Extended    x -> x ^. mode
        Interpreted x -> x ^. mode
        Slash       x -> x ^. mode
      set chord m = case chord of
        Extended    x -> Extended    (mode.~ m $ x)
        Interpreted x -> Interpreted (mode.~ m $ x)
        Slash       x -> Slash       (mode.~ m $ x)
instance ExtendedClass Chord where
  exts = lens get set
    where
      get = \case
        Extended    x -> x ^. exts
        Interpreted x -> x ^. exts
        Slash       x -> x ^. exts
      set chord e = case chord of
        Extended    x -> Extended     (exts .~ e $ x)
        Interpreted x -> Interpreted  (exts .~ e $ x)
        Slash       x -> Slash        (exts .~ e $ x)
makePrisms ''Chord


--Smart Constructors for Chords
data ChordErr c
  = ErrToneConflict Degree c
  | ErrToneNotFound Degree c
  | ErrAddDegreeInvalid Degree
    deriving (Eq, Show)



{-

-}
addExtension :: (ExtendedClass c, Eq c, Show c) => c -> Extension -> Either (ChordErr c) c
addExtension chord ext =
  let
    degreeInChord d =
      if any (^._Accidental.to (d==)) (chordDegrees chord)
        then Right ()
        else Left (ErrToneNotFound deg chord)

    degreeNotInChord d =
      if not $ all (^._Accidental.to (d==)) (chordDegrees chord)
        then Right ()
        else Left (ErrToneConflict deg chord)

    degreeNotInExts d =
      if not $ anyOf (exts.traverse.degree) (d==) chord
        then Right ()
        else Left (ErrToneNotFound deg chord)

    validAccidental d = if deg `elem` [d3, d5, d6, d7, d9, d11, d13]
      then Right ()
      else Left (ErrAddDegreeInvalid d)

    validChord = Right (over exts (ext:) chord)

    deg = ext^.degree
  in case ext of
    Acc acc -> do
      validAccidental deg
      degreeNotInChord deg
      validChord
    Sus2 -> do
      degreeNotInChord d3
      degreeInChord d2
      validChord
    Sus4 -> do
      degreeNotInChord d3
      degreeInChord d4
      validChord
    No n -> do
      degreeInChord n
      degreeNotInExts deg
      validChord

extended
  :: PitchClass
  -> Mode
  -> [Extension]
  -> Either (ChordErr ExtendedChord) ExtendedChord
extended root mode exts = foldM addExtension
  (ExtendedChord root mode []) exts

interpreted
  :: Accidental Degree
  -> Mode
  -> [Extension]
  -> Either (ChordErr InterpretedChord) InterpretedChord
interpreted deg mode exts = foldM addExtension
  (InterpretedChord deg mode []) exts

slash
  :: ExtendedChord
  -> PitchClass
  -> Either (ChordErr SlashChord) SlashChord
slash chord bass = pure (SlashChord chord bass)


--useful for initialising slash chords, eg. C / fMajor creates a slash chord
(/) = slash

toPitches :: Keyed k Chord => k Chord -> [Pitch]
toPitches = error "not yet implemented"
