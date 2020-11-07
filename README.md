
# Datatypes

```
data NoteClass = ...
data Note = Note NoteClass Int

data Quality = Major | Minor | Diminished | Augmented
newtype Degree = Degree Int
newtype Interval = Interval Quality Degree

newtype Scale = Scale [Interval]

data ChordSymbol = ChordSymbol NoteClass Quality [Interval]
data Chord = Chord [Interval]

class Slash s = ChordLike s => Slash { bass :: NoteType s, chord :: s }
```

## Chords

# Classes

## ScaleLike
```
class ScaleLike s
  type NoteType s = *
  interval :: Interval -> Traversal' s Interval
  arpeggiate :: s -> [NoteType s]
instance ScaleLike Scale
instance ScaleLike ChordSymbol
instance ScaleLike Chord
instance ScaleLike s => ScaleLike (Slash s)
```

## ChordLike
```
class ScaleLike s => ChordLike s
instance ChordLike ChordSymbol
instance ChordLike Chord
instance ChordLike s => ChordLike (Slash s)
```

## Degree
```
degree :: ScaleLike s a => Degree -> Traversal' s a
```

## Transpose
```
class Semitones t
  steps :: t -> Int

class Transpose t where
  shift :: Integral n => n -> t -> t
```

## Lenses
```
root :: ScaleLike s => Traversal' s (NoteType s)
quality :: ChordLike s => Traversal' s Quality
exts :: ChordLike s => Traversal' s (NoteType s)
bass :: SlashChord s -> Traversal s (NoteType s)
```
