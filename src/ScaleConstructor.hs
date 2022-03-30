module ScaleConstructor
  ( createMajScale,
    createMinScale,
    createWholeToneScale,
  )
where

import Euterpea (AbsPitch, Music, transpose)
import System.Random (Random (randomRs), mkStdGen)
import System.Random.SplitMix (mkSMGen)

-- Step-Pattern list
majScaleSteps :: [AbsPitch]
majScaleSteps = [0, 2, 4, 5, 7, 9, 11, 12]

minScaleSteps :: [AbsPitch]
minScaleSteps = [0, 2, 3, 5, 7, 8, 10, 11, 12]

wtScaleSteps :: [AbsPitch]
wtScaleSteps = [0, 2 .. 12]

-- Generate Scales from Steps and Key
createScale :: Music a -> [AbsPitch] -> [Music a]
createScale root = map (`transpose` root)

createMajScale :: Music a -> [Music a]
createMajScale root = createScale root majScaleSteps

createMinScale :: Music a -> [Music a]
createMinScale root = createScale root minScaleSteps

createWholeToneScale :: Music a -> [Music a]
createWholeToneScale root = createScale root wtScaleSteps

createRandomScale :: Music a -> [Music a]
createRandomScale root = createScale root randomSteps
  where
    randomSteps = randomRs (0, 12) (mkSMGen 400)

-- TODO: Eventually abstract this out into a new "type" that could implement:
-- data AbsPitchScale = Ionian | Dorian | ..
