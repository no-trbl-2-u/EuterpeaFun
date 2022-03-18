module Main where

import Euterpea

createMajorChord :: (Octave -> Dur -> Music Pitch) -> Music Pitch
createMajorChord root =
  root 4 qn :=: third :=: fifth
  where
    third = transpose 4 (root 4 qn)
    fifth = transpose 7 (root 4 qn)

createMajorChord :: Music a -> Music a
createMajorChord root =
  root :=: third :=: fifth
  where
    third = transpose 4 root
    fifth = transpose 7 root

createMinorChord :: Music a -> Music a
createMinorChord root =
  root :=: third :=: fifth
  where
    third = transpose 3 root
    fifth = transpose 7 root
