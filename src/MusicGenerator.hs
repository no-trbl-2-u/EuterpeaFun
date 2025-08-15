module MusicGenerator
  ( Pattern,
    PatternSet,
    createGenFromScaleAndMode,
    createPattern,
    genFromPattern,
    randomGen,
    finalImplementation,
  )
where

import Euterpea
  ( AbsPitch,
    Dur,
    InstrumentName (..),
    Music (..),
    Pitch (..),
    PitchClass (..),
    Volume,
    dqn,
    dwn,
    en,
    hn,
    instrument,
    line,
    note,
    play,
    qn,
    rest,
    tempo,
    wn,
    (=:),
  )
import Helpers (choose, findInsts)
import Scales (ScaleMode (..), genScale, genScaleNotes)
import System.Random (Random (randomR), RandomGen)
import System.Random.SplitMix (SMGen, mkSMGen)

type Pattern = [AbsPitch] -- Interval patterns (ie. [0, 3] in C is C up to E)

type PatternSet = [Pattern] -- Collection of patterns

type PitchSpace = [AbsPitch] -- Collection of AbsPitches

type PatternInst = [AbsPitch]

type DistThresh = AbsPitch

type Root = AbsPitch

-- | Core pattern generation function using interval patterns and pitch space
genFromPattern :: (RandomGen g) => PitchSpace -> PatternSet -> AbsPitch -> DistThresh -> g -> [AbsPitch]
genFromPattern s k root d g0 =
  pInst ++ genFromPattern s k (last pInst) d g2
  where
    (p, g1) = choose k g0 -- select a pattern to use, pat
    patts = findInsts s p -- find p's instances in s
    nearby someInst =
      abs (head someInst - root) <= d
        && head someInst /= root
    piNear = filter nearby patts -- which instances are nearby (within d half steps)?
    (pInst, g2) =
      if not (null piNear)
        then choose piNear g1 -- pick nearby instances available
        else choose patts g1 -- no nearby instances available

-- | Simple randomization function for pitches and durations
randomGen :: (RandomGen g) => [AbsPitch] -> [Dur] -> Int -> g -> Music (AbsPitch, Volume)
randomGen pitches durs thresh g0 =
  let (p, g1) = choose pitches g0
      (d, g2) = choose durs g1
      (v, g3) = randomR (0, 127) g2
      x =
        if v < thresh
          then rest d
          else note d (p, v)
   in x :+: randomGen pitches durs thresh g3

-- | Creates a generator function from a tonic and mode
-- Usage: createGenFromScaleAndMode C Ionian
createGenFromScaleAndMode :: PitchClass -> ScaleMode -> (PatternSet -> [Dur] -> [AbsPitch])
createGenFromScaleAndMode tonic mode = \patterns noteLengths ->
  let rootNote = 60 + fromEnum tonic -- Middle C = 60, adjust for other tonics
      scaleNotes = genScaleNotes rootNote mode
      -- Generate pattern with a reasonable number of notes
      patternLength = length noteLengths * 4 -- Generate enough notes
      generatedPattern = take patternLength $ genFromPattern scaleNotes patterns rootNote 4 (mkSMGen 300)
   in generatedPattern

-- | Creates a pattern from pattern sets and note lengths
-- Usage: createPattern [[1, 5, 1], [5, 7, 5]] [qn, hn, en]
createPattern :: PatternSet -> [Dur] -> [AbsPitch]
createPattern patterns noteLengths =
  let -- Use C Ionian as default if not specified
      generator = createGenFromScaleAndMode C Ionian
   in generator patterns noteLengths

-- | Final implementation function as requested by the user
-- Usage: finalImplementation patternSet mode
finalImplementation :: PatternSet -> ScaleMode -> IO ()
finalImplementation patternSet mode = do
  let generator = createGenFromScaleAndMode C mode
      pattern0 = take 32 $ generator patternSet [qn, hn, en, qn]
      pattern1 = take 16 $ generator [[1, 3, 5], [5, 3, 1]] [dqn, hn]

      melody0 = line $ map (note qn) pattern0
      melody1 = line $ map (note dqn . (+ 24)) pattern0

  play $
    tempo
      0.50
      ( instrument Cello melody0
          :=: instrument Violin melody1
      )