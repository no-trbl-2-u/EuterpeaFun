module ScaleConstructor where

import ChordConstructor
import Euterpea
import Helpers
import System.Random (Random (randomRs), mkStdGen)
import System.Random.SplitMix (mkSMGen)

-- Type Aliases
type ChordType = String -- "M", "m", "a", "d"

-- Generate Scales from Steps and Key
createScaleFromSteps :: Music a -> [AbsPitch] -> [Music a]
createScaleFromSteps root = map (`transpose` root)

createMajScale :: Music a -> [Music a]
createMajScale root = createScaleFromSteps root majScaleSteps
  where
    majScaleSteps = [0, 2, 4, 5, 7, 9, 11, 12]

createMinScale :: Music a -> [Music a]
createMinScale root = createScaleFromSteps root minScaleSteps
  where
    minScaleSteps = [0, 2, 3, 5, 7, 8, 10, 11, 12]

createWholeToneScale :: Music a -> [Music a]
createWholeToneScale root = createScaleFromSteps root wtSteps
  where
    wtSteps = [0 .. 12]

createRandomScale :: Music a -> [Music a]
createRandomScale root = createScaleFromSteps root randomSteps
  where
    randomSteps = randomRs (0, 12) (mkSMGen 400)
