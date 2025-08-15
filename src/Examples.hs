module Examples where

import ChordConstructor
import Euterpea
  ( AbsPitch,
    Dur,
    InstrumentName (Vibraphone),
    Music (..),
    Pitch,
    PitchClass (C),
    a,
    b,
    c,
    chord,
    d,
    dqn,
    e,
    en,
    f,
    fs,
    g,
    hn,
    instrument,
    line,
    note,
    play,
    qn,
    tempo,
    wn,
  )
import MusicGenerator (PatternSet, createGenFromScaleAndMode, createPattern, finalImplementation)
import Scales (ScaleMode (..), createMinScale)
import System.Random.SplitMix (mkSMGen)

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
cMajorHarmonyExample = play $ melody :=: harmony
  where
    melody = line $ majTriadSet (c 4 qn)
    harmony = chord $ majTriadSet (c 3 qn)

composedPitchSet :: IO ()
composedPitchSet = play $ line pitchSet
  where
    pitchSet = majTriadSet (c 4 qn) ++ majTriadSet (d 2 qn)

bMajorInversionsExample :: IO ()
bMajorInversionsExample =
  play $
    chord root
      :+: chord fstInversion
      :+: chord sndInversion
      :+: chord upperRoot
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

-- ------------------------ New Pattern Generator Examples ------------------------
-- Example pattern sets for demonstration
patternSet0, patternSet1, patternSet2 :: PatternSet
patternSet0 = [[1, 3, 5], [5, 3, 1], [1, 3, 7]]
patternSet1 = [[1, 5, 3, 5], [5, 3, 5, 1], [1, 6, 5]]
patternSet2 = [[1, 5, 3, 5], [7, 5, 7], [5, 8], [8, 7, 1], [1, 8, 7], [8, 5], [7, 3, 7]]

-- | Example using the new createGenFromScaleAndMode API
-- Creates a C Ionian generator
genPatternWithCIonian :: PatternSet -> [Dur] -> [AbsPitch]
genPatternWithCIonian = createGenFromScaleAndMode C Ionian

-- | Simple pattern creation example
playSimplePattern :: IO ()
playSimplePattern = do
  let pattern = createPattern patternSet0 [qn, hn, en]
      melody = line $ map (note qn) (take 16 pattern)
  play $ tempo 1 (instrument Vibraphone melody)

-- | Example using different modes
playModalExample :: IO ()
playModalExample = do
  finalImplementation patternSet1 Dorian

-- | Another modal example
playPhrygianExample :: IO ()
playPhrygianExample = do
  finalImplementation patternSet2 Phrygian

-- | Mixolydian example
playMixolydianExample :: IO ()
playMixolydianExample = do
  finalImplementation patternSet0 Mixolydian

-- | Demo function to showcase the new API
demoNewAPI :: IO ()
demoNewAPI = do
  putStrLn "Playing C Ionian pattern..."
  finalImplementation patternSet0 Ionian
  putStrLn "Playing D Dorian pattern..."
  finalImplementation patternSet1 Dorian
  putStrLn "Playing E Phrygian pattern..."
  finalImplementation patternSet2 Phrygian
