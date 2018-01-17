{-# LANGUAGE MultiParamTypeClasses #-}

module Enpassing.Playable (
  Playable,
  play,
  play_click,
  play_drum
) where

import           Enpassing.Music
import qualified Euterpea.IO.MIDI.Play as MIDI
import           Euterpea.Music

class Playable p where
  as_music :: p -> Music1

instance Playable Sheet where
  as_music (Sheet name key bars) = toMusic1.  line . map Prim $ concat bars

play :: Playable p => p -> IO ()
play = MIDI.play . as_music

play_click :: Playable p => p -> IO ()
play_click playable = MIDI.play $ (toMusic1 click_track) /=: (toMusic1 $ as_music playable)
  where click_track = forever $ perc AcousticSnare    qn

play_drum :: Playable p => p -> IO ()
play_drum playable = MIDI.play . controls$ (toMusic1 drum_track) /=: (toMusic1 $ as_music playable)
  where drum_track = let bass  = perc AcousticBassDrum qn
                         snare = perc AcousticSnare    qn
                         hihat = perc ClosedHiHat      qn
                     in forever $ (hihat :=: bass) :+: hihat :+: (hihat :=: snare) :+: hihat

controls = Modify (Tempo 1.0) . Modify (Instrument PercussiveOrgan)
