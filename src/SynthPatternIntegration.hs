module SynthPatternIntegration where

import BasicSynth
import Control.Concurrent (threadDelay)
import Euterpea (Pitch, absPitch)
import Helpers (choose)
import MusicGenerator (genFromPattern)
import Scales (ScaleMode (..))
import System.Random.SplitMix (SMGen, mkSMGen)

type AbsPitch = Int

-- | Convert MIDI note number to frequency in Hz
-- This is the standard formula: 440 * 2^((n-69)/12)
midiToFreq :: AbsPitch -> Double
midiToFreq midiNum = 440.0 * (2 ** ((fromIntegral midiNum - 69.0) / 12.0))

-- | Play a pattern using the synthesizer
playSynthPattern :: [[AbsPitch]] -> ScaleMode -> IO ()
playSynthPattern patternSet mode = do
  -- Generate a pattern in C major scale
  let pitchSpace = [60, 62, 64, 65, 67, 69, 71, 72] -- C major scale MIDI numbers
      pattern' = take 16 $ genFromPattern pitchSpace patternSet 60 5 (mkSMGen 300)

  -- Play each note in the pattern with the synthesizer
  playPatternWithSynth pattern'
  where
    playPatternWithSynth [] = return ()
    playPatternWithSynth (midiNote : rest) = do
      -- Convert MIDI note to frequency
      let freq = midiToFreq midiNote

      -- Create synth with current frequency
      startSynth
        freq
        0.5
        Sine -- Osc1 using the pattern frequency
        (freq / 2)
        0.3
        Square -- Osc2 one octave below
        5.0
        Triangle -- LFO
        0.7
        0.3 -- Mixer
        1000
        0.3
        True -- Filter
        0.2
        1.0 -- Reverb
        0.8 -- Main volume

      -- Print the current note being played
      putStrLn $ "Playing note: " ++ show midiNote ++ " (Frequency: " ++ show freq ++ " Hz)"

      -- Wait before playing the next note
      threadDelay 500000 -- 500ms

      -- Continue with the rest of the pattern
      playPatternWithSynth rest

-- | Create different synth variations based on a pattern
playSynthVariations :: [[AbsPitch]] -> IO ()
playSynthVariations patternSet = do
  -- Generate a pattern in C major scale
  let pitchSpace = [48, 50, 52, 53, 55, 57, 59, 60] -- C3 major scale MIDI numbers
      pattern' = take 12 $ genFromPattern pitchSpace patternSet 48 7 (mkSMGen 400)

  -- Play each note with varying synthesizer parameters
  playVaryingSynth pattern' 0
  where
    -- Different oscillator types to cycle through
    oscTypes = [Sine, Square, Sawtooth, Triangle]

    playVaryingSynth [] _ = return ()
    playVaryingSynth (midiNote : rest) index = do
      -- Convert MIDI note to frequency
      let freq = midiToFreq midiNote

          -- Choose oscillator types based on position in sequence
          osc1Type = oscTypes !! (index `mod` length oscTypes)
          osc2Type = oscTypes !! ((index + 2) `mod` length oscTypes)

          -- Vary filter based on note pitch
          filterFreq = 500 + fromIntegral (midiNote - 48) * 200

      -- Create synth with varying parameters
      startSynth
        freq
        0.6
        osc1Type -- Osc1 using the pattern frequency
        (freq * 1.01)
        0.4
        osc2Type -- Osc2 slightly detuned
        (0.5 + fromIntegral index * 0.2)
        Triangle -- LFO speed varies
        0.6
        0.4 -- Mixer
        filterFreq
        0.4
        True -- Filter freq varies with note
        (0.1 + fromIntegral (index `mod` 5) * 0.1)
        2.0 -- Varying reverb
        0.8 -- Main volume

      -- Print the current settings
      putStrLn $
        "Note: "
          ++ show midiNote
          ++ " | Osc1: "
          ++ show osc1Type
          ++ " | Osc2: "
          ++ show osc2Type
          ++ " | Filter: "
          ++ show filterFreq
          ++ " Hz"

      -- Wait before playing the next note
      threadDelay 600000 -- 600ms

      -- Continue with the rest of the pattern
      playVaryingSynth rest (index + 1)