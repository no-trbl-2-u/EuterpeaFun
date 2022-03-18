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

-- ------------------------ Examples ------------------------

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

-- ##################### Generate Scales from Steps and Key #####################
-- TODO: Move these to another module!
createMajScale :: Music a -> [Music a]
createMajScale root = map (`transpose` root) majScaleSteps
  where
    majScaleSteps = [0, 2, 4, 5, 7, 9, 11, 12]

createMinScale :: Music a -> [Music a]
createMinScale root = map (`transpose` root) minScaleSteps
  where
    minScaleSteps = [0, 2, 3, 5, 7, 8, 10, 11, 12]

createRandomScale :: Music a -> [Music a]
createRandomScale root = map (`transpose` root) randomSteps
  where
    -- TODO: Figure out some way to do some random stuff here?!
    randomSteps = [0, 2 .. 20]

invertTriadSet :: String -> [Music a] -> [Music a]
invertTriadSet invType noteSet =
  case invType of
    "fst" -> tail noteSet ++ [transpose 12 $ head noteSet]
    "snd" -> last noteSet : map (transpose 12) (tail $ reverse noteSet)
    _ -> noteSet

-- ------------------------ Examples ------------------------
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

playCMajNormalThanInverted :: IO ()
playCMajNormalThanInverted = do
  play $
    chord root :+: cMajFst :+: cMajSnd :+: chord (majTriadSet $ c 4 qn)
  where
    root = majTriadSet $ c 3 qn
    cMajFst = chord $ invertTriadSet "fst" root
    cMajSnd = chord $ invertTriadSet "snd" root

inverted :: String -> Music Pitch
inverted t = line $ invertTriadSet t $ majTriadSet (c 4 qn)

normal :: Music Pitch
normal = line $ majTriadSet (c 4 qn)
