module Music.Theory.MusicalCtx where

import Music.Theory.Key

import Data.Has

type Ctx r a = (r, a)

data KeySig = KeySig'
type instance TypeOf KeySig = Key
