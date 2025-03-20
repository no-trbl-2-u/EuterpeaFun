module PatternPlayer (
  playGenFromSet,
  playGenFromSetAndMode
) where

import           Control.Concurrent     (threadDelay)
import           Euterpea
import           Exercises              (ScaleMode (..), genScale)
import           Helpers                (choose, chooseMany)
import           PatternGenerator       (genFromPattern)
import           System.Random.SplitMix (SMGen, mkSMGen)

-- | Play a pattern generated from a set of patterns
-- This function takes a pattern set and generates music from it
playGenFromSet :: [[AbsPitch]] -> IO ()
playGenFromSet patternSet = do
  play $ tempo 1 (instrument Vibraphone melody)
  where
    -- Generate the melody using the pitchSpace and patternSet
    melody = line $ map (note en) pGenExampleSet

    -- Generate a pattern starting at MIDI note 40 (E2)
    -- with a threshold of 2 and a specific random seed
    pGenExampleSet = genFromPattern pitchSpace patternSet 40 2 (mkSMGen 300)

    -- Create a pitch space for the pattern generator to use
    -- This is a C major scale with limited notes (and extra octave)
    -- TODO: Turn this into a variable (scale)
    pitchSpace = [40, 44, 45, 47, 49] ++ map (+ 12) [40, 44, 45, 47, 49]

-- | Play a pattern generated from a set and a specific scale mode
-- This function takes a pattern set and a scale mode and generates
-- more complex music with counterpoint
playGenFromSetAndMode :: [[AbsPitch]] -> ScaleMode -> IO ()
playGenFromSetAndMode patternSet mode = do
  play $
    tempo
      0.50
      ( instrument Cello bassline0
          :=: instrument Violin bassline1
          :=: instrument Cello melody0
          :=: instrument Violin melody1
      )
  where
    -- Create two melodies at different octaves
    melody0 = line $ map (note qn . (+ 12)) pGenExampleSet0
    melody1 = line $ map (note dqn . (+ 24)) pGenExampleSet0

    -- Create two basslines at different octaves
    bassline0 = line $ map (note dwn) pGenExampleSet1
    bassline1 = line $ map (note wn . (+ 12)) pGenExampleSet1

    -- Generate patterns using the chosen scale mode
    -- The first pattern uses the main pattern set
    pGenExampleSet0 = genFromPattern (genScale (C, 3) mode) patternSet 30 4 (mkSMGen 300)

    -- The second pattern uses a simpler pattern set for the bassline
    -- We use a different seed to create variation
    pGenExampleSet1 = genFromPattern (genScale (C, 2) mode) bassPatterns 30 4 (mkSMGen 400)

    -- Simple pattern set for the bassline
    bassPatterns = [[1, 3, 5], [5, 3, 1], [1, 3, 7]]

-- ########### How to use these functions ###########
-- import PatternPlayer
-- import Exercises (ScaleMode(..))

-- -- Example pattern sets from your Examples.hs
-- patternSet0 = [[1, 3, 5], [5, 3, 1], [1, 3, 7]]
-- patternSet1 = [[1, 5, 3, 5], [5, 3, 5, 1], [1, 6, 5]]
-- patternSet2 = [[1, 5, 3, 5], [7, 5, 7], [5, 8], [8, 7, 1], [1, 8, 7], [8, 5], [7, 3, 7]]

-- main :: IO ()
-- main = do
--   -- Play a simple pattern
--   playGenFromSet patternSet1

--   -- Play a complex pattern with a Dorian mode
--   playGenFromSetAndMode patternSet2 Dorian'

--   -- Connect to the synthesizer (if you have BasicSynth imported)
--   playSynthPattern patternSet1 Mixolydian'
