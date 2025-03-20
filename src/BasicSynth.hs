module BasicSynth
  ( Oscillator(..)
  , OscType(..)
  , Mixer(..)
  , Filter(..)
  , Reverb(..)
  , Synth(..)
  , createOscillator
  , mixSignals
  , applyFilter
  , applyReverb
  , applyFinalVolume
  , startSynth
  ) where

import           Euterpea (Music, Pitch, PitchClass (C), note, qn)
-- import qualified Data.List as L

-- |Different oscillator types that can be used
data OscType = Sine | Square | Sawtooth | Triangle
  deriving (Show, Eq)

-- |Oscillator model with frequency, amplitude and waveform type
data Oscillator = Oscillator
  { oscFreq :: Double   -- Frequency in Hz
  , oscAmp  :: Double   -- Amplitude (0.0-1.0)
  , oscType :: OscType  -- Oscillator type/waveform
  } deriving (Show)

-- |Mixer combines two signals with individual volume controls
data Mixer = Mixer
  { osc1Vol :: Double  -- Volume for oscillator 1 (0.0-1.0)
  , osc2Vol :: Double  -- Volume for oscillator 2 (0.0-1.0)
  } deriving (Show)

-- |Filter with configurable frequency and resonance
data Filter = Filter
  { filtFreq    :: Double  -- Filter cutoff frequency
  , filtRes     :: Double  -- Filter resonance (0.0-1.0)
  , isLFOActive :: Bool -- Whether LFO modulates filter frequency
  } deriving (Show)

-- |Reverb effect with configurable mix and decay
data Reverb = Reverb
  { reverbMix   :: Double  -- Dry/wet mix (0.0-1.0)
  , reverbDecay :: Double  -- Decay time in seconds
  } deriving (Show)

-- |Complete synthesizer configuration
data Synth = Synth
  { osc1      :: Oscillator
  , osc2      :: Oscillator
  , osc3      :: Oscillator  -- LFO
  , mixer     :: Mixer
  , filterVal :: Filter
  , reverb    :: Reverb
  , mainVol   :: Double      -- Main output volume (0.0-1.0)
  } deriving (Show)

-- |Creates an oscillator with the specified parameters
createOscillator :: Double -> Double -> OscType -> Oscillator
createOscillator = Oscillator

-- |Generates a sample for a given oscillator at the specified time
generateSample :: Oscillator -> Double -> Double
generateSample osc time =
  let freq = oscFreq osc
      amp = oscAmp osc
      phase = freq * time
  in amp * case oscType osc of
       Sine     -> sin (2 * pi * phase)
       Square   -> if sin (2 * pi * phase) >= 0 then 1.0 else -1.0
       Sawtooth -> 2 * (phase - fromIntegral (floor phase)) - 1
       Triangle -> 2 * abs (2 * (phase - fromIntegral (floor (phase + 0.25))) - 1) - 1

-- |Mixes two oscillator signals according to the mixer settings
mixSignals :: Oscillator -> Oscillator -> Mixer -> [Double] -> [Double]
mixSignals osc1 osc2 mixer timePoints =
  let samples1 = map (generateSample osc1) timePoints
      samples2 = map (generateSample osc2) timePoints
      vol1 = osc1Vol mixer
      vol2 = osc2Vol mixer
  in zipWith (\s1 s2 -> s1 * vol1 + s2 * vol2) samples1 samples2

-- |Applies a low-pass filter to the audio signal
applyFilter :: Filter -> Oscillator -> [Double] -> [Double]
applyFilter filt lfo samples =
  let baseFreq = filtFreq filt
      res = filtRes filt
      useLFO = isLFOActive filt

      -- This is a simplified IIR filter implementation
      applyFilterSample :: Double -> Double -> Double -> Double -> (Double, Double, Double)
      applyFilterSample input prevOut prevPrevOut cutoff =
        let norm = 1.0 / (1.0 + res * cutoff + cutoff * cutoff)
          in let newOut = (input + 2 * prevOut - prevPrevOut) * norm
             in (newOut, newOut, prevOut)

      -- Process samples with the filter
      processSignal :: [Double] -> [Double] -> [Double]
      processSignal [] _ = []
      processSignal _ [] = []
      processSignal (s:ss) (t:ts) =
        let cutoffFreq = if useLFO
                          then baseFreq + generateSample lfo t * baseFreq * 0.5
                          else baseFreq
            -- This would be the actual filter logic in a real implementation
            filteredSample = s * (0.5 + cutoffFreq/20000)  -- Simplified for demonstration
        in filteredSample : processSignal ss ts

      -- Generate time points for the LFO
      timePoints = [0.0, 1.0/44100 ..] -- Assuming 44.1kHz sample rate

  in processSignal samples (take (length samples) timePoints)

-- |Applies reverb effect to the audio signal
applyReverb :: Reverb -> [Double] -> [Double]
applyReverb rev signal =
  let mix = reverbMix rev
      decay = reverbDecay rev

      -- Simple convolution reverb simulation
      delaySamples = round (decay * 44100) -- Convert decay time to samples
      impulseResponse = [exp (-(t/decay)) | t <- [0.0, 1.0/44100 .. decay]]

      -- Apply impulse response (simplified convolution)
      wetSignal = convolve signal impulseResponse

      -- Mix dry and wet signals
      mixedSignal = zipWith (\dry wet -> (1-mix)*dry + mix*wet) signal wetSignal
  in mixedSignal

  where
    -- Simple convolution function (would be more efficient with FFT in practice)
    convolve :: [Double] -> [Double] -> [Double]
    convolve signal impulse =
      let paddedSignal = signal ++ replicate (length impulse) 0
          result i = sum $ zipWith (*) impulse (take (length impulse) (drop i paddedSignal))
      in map result [0..length signal - 1]

-- |Applies final volume adjustment to the signal
applyFinalVolume :: Double -> [Double] -> [Double]
applyFinalVolume volume = map (* volume)

-- |Processes a complete audio signal through the synth
processSynthSignal :: Synth -> [Double] -> [Double]
processSynthSignal synth timePoints =
  let -- Extract components
      oscillator1 = osc1 synth
      oscillator2 = osc2 synth
      lfo = osc3 synth
      mixerUnit = mixer synth
      filterUnit = filterVal synth
      reverbUnit = reverb synth
      outputVolume = mainVol synth

      -- Apply signal chain
      mixedSignal = mixSignals oscillator1 oscillator2 mixerUnit timePoints
      filteredSignal = applyFilter filterUnit lfo mixedSignal
      reverbedSignal = applyReverb reverbUnit filteredSignal
      finalSignal = applyFinalVolume outputVolume reverbedSignal
  in finalSignal

-- |Converts a synth configuration to a Music Pitch that can be played
synthToMusic :: Synth -> Music Pitch
synthToMusic synth =
  -- This is a placeholder for converting processed samples to a Music value
  -- In a real implementation, this would use Euterpea's underlying Audio type
  let dummyMusic = note qn (C, 4) -- Just a placeholder
  in dummyMusic

-- |Creates and starts the synthesizer with the given parameters
startSynth :: Double -> Double -> OscType -> -- Osc1 params
              Double -> Double -> OscType -> -- Osc2 params
              Double -> OscType ->           -- LFO (Osc3) params
              Double -> Double ->            -- Mixer params
              Double -> Double -> Bool ->    -- Filter params
              Double -> Double ->            -- Reverb params
              Double ->                      -- Main volume
              IO ()
startSynth
  osc1Freq osc1Amp osc1Type
  osc2Freq osc2Amp osc2Type
  osc3Freq osc3Type
  mix1Vol mix2Vol
  filtFrequency filtResonance lfoActive
  revMix revDecay
  volume = do

    let oscillator1 = createOscillator osc1Freq osc1Amp osc1Type
        oscillator2 = createOscillator osc2Freq osc2Amp osc2Type
        lfo = createOscillator osc3Freq 1.0 osc3Type
        mixerUnit = Mixer mix1Vol mix2Vol
        filterUnit = Filter filtFrequency filtResonance lfoActive
        reverbUnit = Reverb revMix revDecay

        synth = Synth
                oscillator1
                oscillator2
                lfo
                mixerUnit
                filterUnit
                reverbUnit
                volume

    putStrLn "Starting synthesizer with the following configuration:"
    putStrLn $ "Oscillator 1: " ++ show oscillator1
    putStrLn $ "Oscillator 2: " ++ show oscillator2
    putStrLn $ "LFO: " ++ show lfo
    putStrLn $ "Mixer: " ++ show mixerUnit
    putStrLn $ "Filter: " ++ show filterUnit
    putStrLn $ "Reverb: " ++ show reverbUnit
    putStrLn $ "Main Volume: " ++ show volume

    -- In a real implementation, this would connect to audio output
    putStrLn "Synthesizer is now running. (This is a simulation)"
