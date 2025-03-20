module SynthExamples where

import Euterpea ( absPitch, PitchClass(F, E, C, B, A, G) )
import BasicSynth
    ( OscType(Sine, Triangle, Sawtooth, Square), startSynth )
import qualified Data.List as L
import Control.Concurrent (threadDelay)

-- Simple example of creating and starting a synthesizer
simpleExample :: IO ()
simpleExample =
  -- Start a synth with default parameters
  startSynth 
    440 0.5 Sine      -- Osc1: A4 note, sine wave, half volume
    220 0.3 Square    -- Osc2: A3 note, square wave, 30% volume
    0.5 Triangle      -- Osc3/LFO: 0.5 Hz triangle wave
    0.7 0.3           -- Mixer: 70% osc1, 30% osc2
    1000 0.3 True     -- Filter: 1kHz cutoff, 30% resonance, LFO active
    0.2 1.5           -- Reverb: 20% wet, 1.5s decay
    0.8               -- Main volume at 80%

-- Example using melody with oscillator frequency modulation
melodyExample :: IO ()
melodyExample = do
  let
    -- Define a sequence of notes to play
    notes = [(C, 4), (E, 4), (G, 4), (C, 5), 
             (B, 4), (A, 4), (G, 4), (F, 4)]
    
    -- Convert to a sequence of MIDI note numbers
    midiNotes = map absPitch notes
    
    -- Play through the melody changing oscillator frequencies
    playSynthWithFreqs [] = return ()
    playSynthWithFreqs (midiNum:rest) = do
      -- Convert MIDI number to frequency: 440 * 2^((n-69)/12)
      let freq = 440 * (2 ** ((fromIntegral midiNum - 69) / 12))
      
      startSynth 
        freq 0.5 Sine          -- Osc1: changing frequency
        (freq/2) 0.3 Sawtooth  -- Osc2: octave below
        2.0 Triangle           -- Osc3/LFO
        0.6 0.4                -- Mixer
        2000 0.4 True          -- Filter
        0.3 1.0                -- Reverb
        0.8                    -- Main volume
      
      -- In a real implementation, we'd wait for the note duration here
      putStrLn $ "Playing frequency: " ++ show freq ++ " Hz"
      threadDelay 500000       -- Wait 500ms between notes
      
      playSynthWithFreqs rest
  
  playSynthWithFreqs midiNotes

-- Example creating a pad sound
padSoundExample :: IO ()
padSoundExample = startSynth 
  440 0.4 Sine      -- Osc1
  443 0.4 Sine      -- Osc2 (slight detuning for thickness)
  0.2 Triangle      -- LFO (slow modulation)
  0.5 0.5           -- Equal mix
  800 0.1 True      -- Low filter with subtle LFO
  0.6 3.0           -- Lots of reverb with long decay
  0.7               -- Moderate volume

-- Example creating a bass sound
bassSoundExample :: IO ()
bassSoundExample = startSynth 
  110 0.7 Sawtooth  -- Osc1 (A2 note)
  110 0.3 Square    -- Osc2 (same note, different waveform)
  5.0 Sine          -- Fast LFO
  0.8 0.2           -- More of the sawtooth
  500 0.7 False     -- Low filter, high resonance, no LFO
  0.1 0.5           -- Minimal reverb
  0.9               -- Loud

-- Example with filter sweeping
filterSweepExample :: IO ()
filterSweepExample = do
  let
    -- Sequence of filter frequencies for a filter sweep
    filterFreqs = [500, 1000, 2000, 4000, 8000, 4000, 2000, 1000, 500]
    
    -- Play through the filter sweep
    playSweep [] = return ()
    playSweep (f:fs) = do
      startSynth 
        440 0.5 Sawtooth  -- Osc1
        220 0.5 Square    -- Osc2
        0.1 Sine          -- Slow LFO
        0.5 0.5           -- Equal mix
        f 0.7 False       -- Changing filter freq, high resonance
        0.2 1.0           -- Some reverb
        0.8               -- Good volume
      
      putStrLn $ "Filter frequency: " ++ show f ++ " Hz"
      threadDelay 500000  -- Wait 500ms between filter changes
      
      playSweep fs
  
  playSweep filterFreqs

-- TODO: Create a pattern for these synths using my own pattern generator
-- Example creating a melody using the pattern generator from my code
patternExample :: IO ()
patternExample = do
  putStrLn "This would use my pattern generators with the synth"
  putStrLn "For example:"
  putStrLn "1. Generate a pattern of notes using genFromPattern"
  putStrLn "2. Convert those notes to frequencies"
  putStrLn "3. Use each frequency to control the synth parameters"
  
  -- This is a placeholder for integration with my pattern generators
  -- In a real implementation, it would loosely look like this:
  -- 
  -- import PatternGenerator (genFromPattern)
  -- import System.Random.SplitMix (mkSMGen)
  --
  -- let patternSet = [[1, 3, 5], [5, 3, 1]]
  --     pitchSpace = [60, 62, 64, 65, 67, 69, 71, 72]  -- C major scale MIDI numbers
  --     pattern = genFromPattern pitchSpace patternSet 60 5 (mkSMGen 300)
  --     
  --     -- Convert pattern to frequencies and play
  --     frequencies = map (\midi -> 440 * (2 ** ((fromIntegral midi - 69) / 12))) pattern
  --     playPattern [] = return ()
  --     playPattern (f:fs) = do
  --       -- Create synth with current frequency
  --       startSynth f 0.5 Sine (f/2) 0.3 Square 1.0 Triangle 0.6 0.4 1000 0.3 True 0.2 1.0 0.8
  --       threadDelay 200000
  --       playPattern fs
  --     
  -- playPattern (take 16 frequencies)  -- Play first 16 notes