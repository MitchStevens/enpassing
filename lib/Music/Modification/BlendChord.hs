module Music.Modification.BlendChord where

import           Music.Theory

import           Control.Lens

newtype Prioritised a = Prioritised { getPriority :: (Int, a) }

type ChordAxiom = Chord -> Any

instance Ord (Prioritised a) where
  compare = compare `on` getPriority

relNote :: Interval -> ChordAxiom
relNote interval = Any . elemOf tones interval

absNote :: Note -> ChordAxiom
absNote note = Any . elemOf notes note

{-
  The process of generalisation
    - get all the chord axioms
    - proiritise the axioms (objective process, 3s and 7s most important etc.
-}
generalisation :: Chord -> [Prioritised ChordAxiom]
generalisation chord = undefined

generalisationPathPrefixs :: Chord -> [[Prioritised ChordAxiom]]
generalisationPathPrefixs = tails . generalisation
  

cost :: f (Prioritised ChordAxiom) -> Int
cost = 


totalCost :: f (Prioritised ChordAxiom) -> f (Prioritised ChordAxiom) -> Int
totalCost p1 p2 = (max (cost p1) (cost p2))^2  + min (cost p1) (cost p2)

chordCompletion :: [ChordAxiom] -> Chord
chordCompletion 

disallowDissonantChord :: ChordAxiom
disallowDissonantChord = fold
  [ not' . (relNote (min iii) <> relNote (maj iii))
  , not' . relNote (min ii)
  , not' . (relNote (dim v) <> relNote (perf v))
  ]
    where not' = Any . not . getAny
