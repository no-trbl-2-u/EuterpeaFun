module PatternGenerator where

import Data.List (nub)
import Euterpea
import System.Random (Random (randomRs), StdGen, mkStdGen)

-- Leverages code and Ideas from -> https://www.youtube.com/watch?v=UVcXNhgVr9o

type PitchSpace = [AbsPitch] -- Collection of AbsPitches

type Pattern = [AbsPitch] -- Interval patterns (ie. [0, 3] in C is C up to E)

type PatternInst = [AbsPitch]

type DistThresh = AbsPitch

type Root = AbsPitch

randomize :: StdGen -> [a] -> [a]
randomize sg rs =
  let n = length rs
      plist = take n (nub (randomRs (0, n -1) sg))
   in map (rs !!) plist

choose :: [a] -> StdGen -> (a, StdGen)
choose items g0 = (head $ randomize g0 items, g0)

findInsts :: PitchSpace -> Pattern -> [PatternInst]
findInsts s pat =
  filter (all (`elem` s)) patInsts
  where
    patInsts = map (\a -> map (+ a) pat) s

pGen :: PitchSpace -> [Pattern] -> AbsPitch -> DistThresh -> StdGen -> Pattern
pGen s k root d g0 =
  pInst ++ pGen s k (last pInst) d g2
  where
    (p, g1) = choose k g0 -- select a pattern to use, pat
    patts = findInsts s p -- find p's instances in s
    nearby someInst =
      abs (head someInst - root) <= d
        && head someInst /= root
    piNear = filter nearby patts -- which instances are nearby (within d half steps)?
    (pInst, g2) =
      -- pick nearby instances available
      if not (null piNear)
        then choose piNear g1
        else -- PROBLEM: We're stuck looping this after initial rise
          choose patts g1 -- no nearby instances available

-- Example values
cMajorScale :: PitchSpace
cMajorScale = scaleOctDown ++ scale ++ scaleOctUp
  where
    scale = [40, 42, 44, 45, 47, 49, 51, 52]
    scaleOctDown = map (\n -> n - 12) scale
    scaleOctUp = map (+ 12) scale

exampleSet :: Pattern
exampleSet = pGen pitchSpace patterns root distThresh seed
  where
    pitchSpace = cMajorScale
    -- pitchSpace = [40 .. 70] :: Pattern
    -- PROBLEM: Pattern switches then loops
    patterns = [[0, 7], [0, 2], [0, 4]] :: [Pattern]
    distThresh = 2 :: DistThresh
    root = 40 :: Root
    seed = mkStdGen 6 :: StdGen

pGenExample :: Music AbsPitch
pGenExample = line $ map (note sn) exampleSet

playPGen :: IO ()
playPGen = do
  play pGenExample
