module Scales
  ( ScaleMode (..),
    createMajScale,
    createMinScale,
    createWholeToneScale,
    genScale,
    genScaleNotes,
  )
where

import Euterpea (AbsPitch, Music, Pitch, absPitch, pitch, trans, transpose)
import System.Random (Random (randomRs), mkStdGen)
import System.Random.SplitMix (mkSMGen)

-- | Scale modes for modal generation
data ScaleMode
  = Ionian
  | Dorian
  | Phrygian
  | Lydian
  | Mixolydian
  | Aeolian
  | Locrian
  deriving (Show, Enum, Eq)

type Step = Int

type PitchSpace = [AbsPitch]

-- Step-Pattern lists
majScaleSteps :: [AbsPitch]
majScaleSteps = [0, 2, 4, 5, 7, 9, 11, 12]

minScaleSteps :: [AbsPitch]
minScaleSteps = [0, 2, 3, 5, 7, 8, 10, 11, 12]

wtScaleSteps :: [AbsPitch]
wtScaleSteps = [0, 2 .. 12]

majorSteps :: [Step]
majorSteps = [2, 2, 1, 2, 2, 2, 1]

-- Generate Scales from Steps and Key
createScale :: Music a -> [AbsPitch] -> [Music a]
createScale root = map (`transpose` root)

createMajScale :: Music a -> [Music a]
createMajScale root = createScale root majScaleSteps

createMinScale :: Music a -> [Music a]
createMinScale root = createScale root minScaleSteps

createWholeToneScale :: Music a -> [Music a]
createWholeToneScale root = createScale root wtScaleSteps

-- | Creates a scale from root and interval pattern
mkScale :: Pitch -> [Step] -> [AbsPitch]
mkScale root [] = [absPitch root]
mkScale root (interval : intervals) =
  absPitch root : mkScale (trans interval root) intervals

-- | Generate a scale in a specific mode from a root pitch
genScale :: Pitch -> ScaleMode -> PitchSpace
genScale p mode = mkScale p (cycleScaleNTimes (fromEnum mode))
  where
    cycleScaleNTimes = (iterate cycleScale majorSteps !!)
    cycleScale xs = [head $ tail xs] ++ tail (tail xs) ++ [head xs]

-- | Generate scale notes (AbsPitch values) for pattern generation
genScaleNotes :: AbsPitch -> ScaleMode -> [AbsPitch]
genScaleNotes rootNote mode =
  let rootPitch = pitch rootNote
      scaleNotes = genScale rootPitch mode
   in scaleNotes ++ map (+ 12) scaleNotes -- Add octave for more range