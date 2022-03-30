module Examples where

import ChordConstructor
import Euterpea
import Exercises
import Helpers (choose)
import PatternGenerator (genFromPattern, randomGen)
import ScaleConstructor (createMinScale)
import System.Random.SplitMix (SMGen, mkSMGen)

-- ------------------------ Triad Progression Examples ------------------------
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
playTriadExample = do
  play $ chordProg1 :+: chordProg1 :+: chordProg2 :+: major (c 4 wn)

-- ------------------------ Triad Set Examples ------------------------
cMajorHarmonyExample :: IO ()
cMajorHarmonyExample = do
  play $ melody :=: harmony
  where
    melody = line $ majTriadSet (c 4 qn)
    harmony = chord $ majTriadSet (c 3 qn)

composedPitchSet :: IO ()
composedPitchSet = do
  play $ line pitchSet
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
  play $ melody :+: harmony
  where
    melody = line . createMinScale $ b 2 qn
    harmony = chord . minTriadSet $ b 2 qn

playDMinScaleArp :: IO ()
playDMinScaleArp = do
  play $ melUp :+: melDown
  where
    melUp = line . createMinScale $ b 2 qn
    melDown = line . reverse . createMinScale $ b 2 qn

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

-- ------------------------ Pattern Generator Examples ------------------------
cMajorScale, cMajorScaleLess, cMajorPhrygian :: [AbsPitch]
cMajorScale = genScale (C, 3) Ionian'
cMajorScaleLess = [40, 44, 45, 47, 49]
cMajorPhrygian = genScale (C, 3) Phrygian'

patternSet0, patternSet1, patternSet2 :: [[AbsPitch]]
patternSet0 = [[1, 3, 5], [5, 3, 1], [1, 3, 7]]
patternSet1 = [[1, 5, 3, 5], [5, 3, 5, 1], [1, 6, 5]]
patternSet2 = [[1, 5, 3, 5], [7, 5, 7], [5, 8], [8, 7, 1], [1, 8, 7], [8, 5], [7, 3, 7]]

playGenFromPattern0 :: [[AbsPitch]] -> IO ()
playGenFromPattern0 patternSet = do
  play $ tempo 1 (instrument Vibraphone melody)
  where
    melody = line $ map (note en) pGenExampleSet
    pGenExampleSet = genFromPattern pitchSpace patternSet0 40 2 (mkSMGen 300)
    pitchSpace = cMajorScaleLess ++ map (+ 12) cMajorScaleLess

playGenFromPattern1 :: [[AbsPitch]] -> ScaleMode -> IO ()
playGenFromPattern1 patternSet mode = do
  play $
    tempo
      0.50
      ( instrument Cello bassline0
          :=: instrument Violin bassline1
          :=: instrument Cello melody0
          :=: instrument Violin melody1
      )
  where
    melody0 = line $ map (note qn . (+ 12)) pGenExampleSet0
    melody1 = line $ map (note dqn . (+ 24)) pGenExampleSet0
    bassline0 = line $ map (note dwn) pGenExampleSet1
    bassline1 = line $ map (note wn . (+ 12)) pGenExampleSet1
    pGenExampleSet0 = genFromPattern (genScale (C, 3) mode) patternSet 30 4 (mkSMGen 300)
    pGenExampleSet1 = genFromPattern (genScale (C, 2) mode) patternSet0 30 4 (mkSMGen 400)

playRandomGen :: IO ()
playRandomGen = do
  play $ tempo 2 (instrument Xylophone melody :=: instrument Vibraphone melody2)
  where
    melody = randomGen (map (+ 12) cMajorScaleLess) [hn, wn, hn, qn] 0 (mkSMGen 500)
    melody2 = randomGen (map (+ 24) cMajorScale) [qn, sn, qn, en] 0 (mkSMGen 600)
