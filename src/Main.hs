module Main where

import ChordConstructor
import Euterpea
import ScaleConstructor

-- :=: -- Harmonize
-- :+: -- Melodize

-- Melody Examples

-- Chord Progression Examples
-- Key of C Maj -> (I : IV : ii : V)
chordProg1 :: Music Pitch
chordProg1 =
  major (c 4 wn)
    :+: major (f 3 qn)
    :+: minor (d 3 qn)
    :+: major (g 3 hn)

-- Key of C Maj -> (vi : ii : V : iv)
chordProg2 :: Music Pitch
chordProg2 =
  minor (a 3 hn)
    :+: minor (e 3 hn)
    :+: major (g 3 hn)
    :+: minor (f 3 hn)

resolution :: Music Pitch
resolution = major (c 4 wn)

-- Final Composition
main :: IO ()
main = do
  play $ chordProg1 :+: chordProg1 :+: chordProg2 :+: resolution
