module PatternPlayer (
  playGenFromSet,
  playGenFromSetAndMode
) where

import Euterpea
import Exercises (ScaleMode(..), genScale)
import PatternGenerator (genFromPattern)
import BasicSynth
import System.Random.SplitMix (SMGen, mkSMGen)
import Control.Concurrent (threadDelay)

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
    -- This is a C major scale with limited notes
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

-- | Connect the pattern player to the synthesizer
-- This is an advanced function that uses the synth to play patterns
-- It demonstrates how to use both systems together
playSynthPattern :: [[AbsPitch]] -> ScaleMode -> IO ()
playSynthPattern patternSet mode = do
  putStrLn "Generating pattern..."
  
  -- Generate the pattern using the specified mode and pattern set
  let modeScale = genScale (C, 3) mode
  let pattern' = genFromPattern modeScale patternSet 60 5 (mkSMGen 300)
  
  putStrLn "Converting to frequencies..."
  
  -- Convert MIDI notes to frequencies
  let toFreq midi = 440 * (2 ** ((fromIntegral midi - 69) / 12))
      frequencies = map toFreq (take 16 pattern')  -- Play first 16 notes
  
  putStrLn "Playing with synthesizer..."
  
  -- Play each note in sequence using the synthesizer
  playWithSynth frequencies
  
  where
    -- Helper function to play a sequence of frequencies with the synth
    playWithSynth [] = putStrLn "Pattern complete"
    playWithSynth (freq:rest) = do
      -- Create a synth with the current frequency
      -- This demonstrates how to connect your pattern generator with your synth
      startSynth 
        freq 0.5 Sine           -- Osc1 follows the pattern
        (freq/2) 0.3 Sawtooth   -- Osc2 plays an octave below
        3.0 Triangle            -- Moderate speed LFO
        0.6 0.4                 -- Mix favoring the main oscillator
        2000 0.4 True           -- Filter with LFO modulation
        0.2 1.0                 -- Light reverb
        0.8                     -- Moderate volume
      
      -- Wait before playing the next note (200ms)
      threadDelay 200000
      
      -- Recursive call to play the rest of the pattern
      playWithSynth rest

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