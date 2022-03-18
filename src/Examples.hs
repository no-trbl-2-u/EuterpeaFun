module Examples where

import ChordConstructor
import Euterpea
import ScaleConstructor

-- ------------------------ Triad Examples ------------------------
-- Chord Progression Examples
-- Key of C Maj -> (I : IV : ii : V)
chordProg1 :: Music Pitch
chordProg1 =
  major (c 4 wn)
    :+: major (f 3 qn)
    :+: minor (d 3 qn)
    :+: major (g 3 hn)

-- Key of C Maj -> (vi : iii : V : iv)
chordProg2 :: Music Pitch
chordProg2 =
  minor (a 3 hn)
    :+: minor (e 3 hn)
    :+: major (g 3 hn)
    :+: minor (f 3 hn)

playTriadExample :: IO ()
playTriadExample =
  do
    play $
      chordProg1 :+: chordProg1 :+: chordProg2 :+: major (c 4 wn)

-- ------------------------ Triad Set Examples ------------------------
cMajorHarmonyExample :: IO ()
cMajorHarmonyExample = do
  play $
    melody :=: harmony
  where
    melody = line $ majTriadSet (c 4 qn)
    harmony = chord $ majTriadSet (c 3 qn)

composedPitchSet :: IO ()
composedPitchSet = do
  play $
    line pitchSet
  where
    pitchSet = majTriadSet (c 4 qn) ++ majTriadSet (d 2 qn)

bMajorInversionsExample :: IO ()
bMajorInversionsExample = do
  play $
    chord root :+: chord fstInversion :+: chord sndInversion :+: chord upperRoot
  where
    root = majTriadSet $ b 3 hn
    fstInversion = invertTriadSet "fst" root
    sndInversion = invertTriadSet "snd" root
    upperRoot = majTriadSet $ b 4 hn

-- ------------------------ Scale Set Examples ------------------------
playBMinScale :: IO ()
playBMinScale = do
  play $
    melody :+: harmony
  where
    melody = line $ createMinScale (b 2 qn)
    harmony = chord $ minTriadSet (b 2 qn)

playDMinScaleArp :: IO ()
playDMinScaleArp = do
  play $
    melUp :+: melDown
  where
    melUp = line $ createMinScale (b 2 qn)
    melDown = line $ reverse $ createMinScale (b 2 qn)

playDMinScaleArpWithRes :: IO ()
playDMinScaleArpWithRes = do
  play $
    melUp :+: melDown :+: lowFive :+: tonic
  where
    root = b 3
    tonic = chord . minTriadSet $ root wn
    lowFive = chord . majTriadSet $ fs 2 hn
    melUp = line . createMinScale $ root qn
    melDown = line . reverse . createMinScale $ root qn
