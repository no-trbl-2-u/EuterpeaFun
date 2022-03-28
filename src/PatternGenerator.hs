module PatternGenerator where

-- Leverages code and Ideas from -> https://www.youtube.com/watch?v=UVcXNhgVr9o
import Euterpea (AbsPitch, Dur, Music ((:+:)), Volume, note, rest)
import Helpers (choose, findInsts)
import System.Random (Random (randomR))
import System.Random.SplitMix (SMGen, mkSMGen)

type PitchSpace = [AbsPitch] -- Collection of AbsPitches

type Pattern = [AbsPitch] -- Interval patterns (ie. [0, 3] in C is C up to E)

type PatternInst = [AbsPitch]

type DistThresh = AbsPitch

type Root = AbsPitch

-- Interval Pattern-based Randomization using a PitchSpace
genFromPattern :: PitchSpace -> [Pattern] -> AbsPitch -> DistThresh -> SMGen -> Pattern
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

-- LESS complex randomization, but randomize duration
randomGen :: [AbsPitch] -> [Dur] -> Int -> SMGen -> Music (AbsPitch, Volume)
randomGen pitches durs thresh g0 =
  let (p, g1) = choose pitches g0
      (d, g2) = choose durs g1
      (v, g3) = randomR (0, 127) g2
      x =
        if v < thresh
          then rest d
          else note d (p, v)
   in x :+: randomGen pitches durs thresh g3
