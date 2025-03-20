module PatternPlayer (
  playGenFromSet,
  playGenFromSetAndMode
) where

import           Euterpea
import           Exercises              (ScaleMode (..), genScale)
import           Helpers                (choose, chooseMany)
import           PatternGenerator       (genFromPattern)
import           System.Random.SplitMix (SMGen, mkSMGen)

-- | Play a pattern generated from a set of patterns
-- This function takes a pattern set and generates music from it
playGenFromSet :: [[AbsPitch]] -> IO ()
playGenFromSet patternSet = do
  -- Create a pitch space for pattern generation (C major scale with limited notes + octave)
  let pitchSpace = [40, 44, 45, 47, 49] ++ map (+ 12) [40, 44, 45, 47, 49]

      -- Generate a pattern starting at MIDI note 40 (E2)
      -- with a threshold of 2 and a specific random seed
      pGenExampleSet = take 32 $ genFromPattern pitchSpace patternSet 40 2 (mkSMGen 300)

      -- Map each note in the generated pattern to a musical note
      melody = line $ map (note en) pGenExampleSet

  -- Play the generated melody with a tempo of 1 and using the Vibraphone instrument
  play $ tempo 1 (instrument Vibraphone melody)

-- | Play a pattern generated from a set and a specific scale mode
-- This function takes a pattern set and a scale mode and generates
-- more complex music with counterpoint
playGenFromSetAndMode :: [[AbsPitch]] -> ScaleMode -> IO ()
playGenFromSetAndMode patternSet mode = do
  -- Simple pattern set for the bassline
  let bassPatterns = [[1, 3, 5], [5, 3, 1], [1, 3, 7]]

      -- Generate patterns using the chosen scale mode
      -- Take a fixed number of notes to ensure the pattern is finite
      pGenExampleSet0 = take 32 $ genFromPattern (genScale (C, 3) mode) patternSet 30 4 (mkSMGen 300)
      pGenExampleSet1 = take 16 $ genFromPattern (genScale (C, 2) mode) bassPatterns 30 4 (mkSMGen 400)

      -- Create two melodies at different octaves
      melody0 = line $ map (note qn . (+ 12)) pGenExampleSet0
      melody1 = line $ map (note dqn . (+ 24)) pGenExampleSet0

      -- Create two basslines at different octaves
      bassline0 = line $ map (note dwn) pGenExampleSet1
      bassline1 = line $ map (note wn . (+ 12)) pGenExampleSet1

  -- Play all parts together with a slower tempo
  play $
    tempo
      0.50
      ( instrument Cello bassline0
          :=: instrument Violin bassline1
          :=: instrument Cello melody0
          :=: instrument Violin melody1
      )
