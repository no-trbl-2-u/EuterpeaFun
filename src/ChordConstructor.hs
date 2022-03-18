module ChordConstructor where

import Euterpea

-- Type Aliases
type ChordType = String -- "M", "m", "a", "d"

-- Helpers
transRoot :: ChordType -> Music a -> Music a
transRoot _ = transpose 0

transThird :: ChordType -> Music a -> Music a
transThird str = transpose thirdValue
  where
    thirdValue = case str of "M" -> 4; "m" -> 3; _ -> 4

transFifth :: ChordType -> Music a -> Music a
transFifth str = transpose fifthValue
  where
    fifthValue = case str of "a" -> 8; "d" -> 6; _ -> 7

-- ######################## Triad Chords ########################
-- Private Triad Constructor Helper
createTriad :: ChordType -> Music a -> Music a
createTriad str root =
  root :=: transThird str root :=: transFifth str root

-- Public Triad Constructors
major :: Music a -> Music a
major = createTriad "M"

minor :: Music a -> Music a
minor = createTriad "m"

aug :: Music a -> Music a
aug = createTriad "a"

dim :: Music a -> Music a
dim = createTriad "d"

-- ######################## Triad Sets ########################
-- line  -> Flatten [Music a] to Melodize Set
-- chord -> Flatten [Music a] to Harmonize Set
-- (++)  -> Concat two sets!

-- Private Triad Set Constructor
createTriadSet :: ChordType -> Music a -> [Music a]
createTriadSet t root = [transRoot t root, transThird t root, transFifth t root]

-- Public Triad
majTriadSet :: Music a -> [Music a]
majTriadSet = createTriadSet "M"

minTriadSet :: Music a -> [Music a]
minTriadSet = createTriadSet "m"

augTriadSet :: Music a -> [Music a]
augTriadSet = createTriadSet "a"

dimTriadSet :: Music a -> [Music a]
dimTriadSet = createTriadSet "d"

invertTriadSet :: String -> [Music a] -> [Music a]
invertTriadSet invType noteSet =
  case invType of
    "fst" -> tail noteSet ++ [transpose 12 $ head noteSet]
    "snd" -> [last noteSet] ++ [transpose 12 $ head noteSet] ++ [transpose 12 . head . tail $ noteSet]
    _ -> noteSet

-- ------------------------ Examples ------------------------

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
