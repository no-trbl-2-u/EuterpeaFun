module Main where

import Euterpea

-- Private Triad Constructor Helper
createTriad :: String -> Music a -> Music a
createTriad str root =
  root :=: transpose thirdValue root :=: transpose fifthValue root
  where
    thirdValue = case str of "M" -> 4; "m" -> 3; _ -> 4
    fifthValue = case str of "a" -> 8; "d" -> 6; _ -> 7

-- Public Triad Constructors
major :: Music a -> Music a
major = createTriad "M"

minor :: Music a -> Music a
minor = createTriad "m"

aug :: Music a -> Music a
aug = createTriad "a"

dim :: Music a -> Music a
dim = createTriad "d"

-- :=: -- Harmonize
-- :+: -- Melodize

-- Melodies

-- Chord Progressions
-- Key of C Maj -> (I : IV : ii : V)
chordProg1 :: Music Pitch
chordProg1 =
  major (c 4 hn)
    :+: major (f 3 hn)
    :+: minor (d 3 qn)
    :+: major (g 3 hn)

-- Key of C Maj -> (vi : ii : V : iv)
chordProg2 :: Music Pitch
chordProg2 =
  minor (a 3 wn)
    :+: minor (d 4 hn)
    :+: major (g 4 qn)
    :+: minor (f 4 hn)

resolution :: Music Pitch
resolution = major (c 4 wn)

-- Final Composition
main :: IO ()
main = do
  play $ chordProg1 :+: chordProg2 :+: resolution
