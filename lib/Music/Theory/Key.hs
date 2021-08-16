module Music.Theory.Key where

import Music.Theory.Accidental
import Music.Theory.Note

data KeyMode = MajorKey | MinorKey
data Key = Key NoteName Accidental KeyMode

pattern CMajorKey  = Key C Natural MajorKey
pattern GMajorKey  = Key G Natural MajorKey
pattern DMajorKey  = Key D Natural MajorKey
pattern AMajorKey  = Key A Natural MajorKey
pattern EMajorKey  = Key E Natural MajorKey
pattern BMajorKey  = Key B Natural MajorKey
pattern FsMajorKey = Key F Sharp   MajorKey

pattern FMajorKey  = Key F Natural MajorKey
pattern BbMajorKey = Key B Flat    MajorKey
pattern EbMajorKey = Key E Flat    MajorKey
pattern AbMajorKey = Key A Flat    MajorKey
pattern DbMajorKey = Key D Flat    MajorKey
pattern GbMajorKey = Key G Flat    MajorKey

countFlats :: Key -> Int
countFlats key = case key of
  FMajorKey  -> 1
  BbMajorKey -> 2
  EbMajorKey -> 3
  AbMajorKey -> 4
  DbMajorKey -> 5
  GbMajorKey -> 6
  Key n _ MinorKey ->
  _ -> 0

countSharps :: Key -> Int
  Key _ _ MinorKey ->
  GMajorKey  -> 1
  DMajorKey  -> 2
  AMajorKey  -> 3
  EMajorKey  -> 4
  BMajorKey  -> 5
  FsMajorKey -> 6
  _ -> 0

class Keyed a where
  key :: a -> Key
