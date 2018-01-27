module Enpassing.Changes.Style where

import           Data.Bifunctor
import           Data.Monoid
import           Enpassing.Changes.Passing
import           Enpassing.Changes.Substitution
import           Enpassing.Music
import           Euterpea.Music
import           Test.QuickCheck.Gen

data Style = Style { substitutions :: [(Int, Substitution)],
                     additions     :: [(Int, PassingChord)] }

instance Monoid Style where
  mempty = Style [] []
  mappend (Style s1 p1) (Style s2 p2) = Style (s1++s2) (p1++p2)

basic_style = Style
  [ (50, no_sub)
  , (20, tritone_sub)
  , (50, sharp_i_replaces_VI) ]
  []

add_substitutions :: Style -> Sheet -> IO Sheet
add_substitutions (Style subs _) = undefined

add_passing_chords :: Style -> Sheet -> IO Sheet
add_passing_chords = undefined

--Substitutions
{-
generate_substitutions :: Sheet -> IO Sheet
generate_substitutions (Sheet name key bars) = Sheet name key <$> substitute (Keyed key bars)
  where
    substitute_list :: (Traversable t) => Keyed (t Chord) -> IO (t Chord)
    substitute_list (Keyed k trav) = mapM (substitute . Keyed k) trav


instance Monoid Substitution where
  mempty = undefined

  mappend s1 s2 = mconcat [s1, s2]

  mconcat list = Substitution name pred d gen
    where
      name = intercalate ", " $ fmap sub_name list
      pred = or $ fmap is_situational list
      d    = sum $ fmap freq list
      gen chord = frequency $ mapMaybe (gen_tuples chord) list

      gen_tuples :: Keyed Chord -> Substitution -> Maybe (Int, Gen Chord)
      gen_tuples chord (Substitution _ p d g) =
        if p chord
          then Just (d, g chord)
          else Nothing

instance (Traversable t, Substitutable s) => Substitutable (t s) where
  substitute (Keyed k trav) = mapM (substitute . Keyed k) trav


-- Substitutables: what
class Substitutable s where
  substitute :: Style -> Keyed s -> IO s

instance Substitutable Chord where
  substitute (Style subs pass) chord = undefined --generate new_generator
    where
      situationals = filter (is_situational chord . snd) :: [(Int, Substitution)]
      --new_generator = frequency $ second generator chord <$> situationals :: Gen Chord


--Passing Additions
--class PassingAddition a b where
  --passing_chords :: Style -> Keyed a -> IO b

instance PassingAddition (Primitive Chord, Primitive Chord) [Primitive Chord]

instance PassingAddition [Primitive Chord] [Primitive Chord]


instance PassingAddition [Primitive Chord] where
  passing_chords (Keyed key list) = sequenceA $ concatMap passing_chord tuples
    where
      PassingChord _ pred gen = no_addition <> tritone_addition
      tuples = zip list (tail $ cycle list)

      passing_chord :: (Primitive Chord, Primitive Chord) -> [Gen (Primitive Chord)]
      passing_chord (Note d x1, Note _ x2) = [pure (Note (d/2) x1), Note (d/2) <$> gen key x1 x2]
      passing_chord (Note d x1, Rest _)    = [pure (Note d x1)]
      passing_chord (Rest d,    _)         = [pure (Rest d)]

      instance Semigroup PassingAddition where
  PassingAddition n1 d1 p1 g1 <> PassingAddition n2 d2 p2 g2 = PassingAddition n3 d3 p3 g3
    where
      n3 = n1++", "++n2
      d3 = d1 + d2
      p3 = mappend p1 p2
      g3 k c1 c2 = frequency [(d1, g1 k c1 c2), (d2, g2 k c1 c2)]-}

add_passing_chord :: Sheet
                  -> Keyed (Primitive Chord, Primitive Chord)
                  -> Gen [Primitive Chord]
add_passing_chord (Sheet _ _ pass) = undefined
