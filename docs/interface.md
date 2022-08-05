# Interface

```
ix :: Index s -> Traversal' s (IxValue s)
ix :: Degree -> Traversal' Mode Interval
ix :: Degree -> Traversal' Scale PitchClass
ix :: Degree -> Traversal' Chord Pitch

interval :: ScaleLike s => Interval -> Traversal' s (IxValue s)
interval :: Interval -> Traversal' Mode Interval
interval :: Interval -> Traversal' Scale PitchClass
interval :: Interval -> Traversal' Chord Pitch

root :: HasRoot s => Lens' s (IxValue s)
root :: Lens' Scale PitchClass
root :: Lens' Chord Pitch

quality :: ScaleLike s => Traversal' s Quality
quality :: Traversal' Mode  Quality
quality :: Traversal' Scale Quality
quality :: Traversal' Chord Quality

exts :: ChordLike s => Traversal' s (IxValue s)
exts :: Traversal' Chord Pitch

accidental :: (HasRoot s, HasAccidental (IxValue s)) => Lens' s Accidental
accidental :: Lens' Scale Accidental
accidental :: Lens' Chord Accidental
```
