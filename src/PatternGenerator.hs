module PatternGenerator where

import Data.List (nub)
import Euterpea
import System.Random (Random (randomR, randomRs), StdGen, mkStdGen, next)

-- import System.Random (next)

-- Leverages code and Ideas from -> https://www.youtube.com/watch?v=UVcXNhgVr9o

type PitchSpace = [AbsPitch] -- Collection of AbsPitches

type Pattern = [AbsPitch] -- Interval patterns (ie. [0, 3] in C is C up to E)

type PatternInst = [AbsPitch]

type DistThresh = AbsPitch

type Root = AbsPitch

-- randomize :: StdGen -> [a] -> [a]
-- randomize sg rs =
--   let n = length rs
--       plist = take n (nub (randomRs (0, n -1) sg))
--    in map (rs !!) plist

choose :: [a] -> StdGen -> (a, StdGen)
choose [] g = error "Nothing to Choose from!"
choose xs g =
  let (i, g') = next g
   in (xs !! (i `mod` length xs), g')

findInsts :: PitchSpace -> Pattern -> [PatternInst]
findInsts s pat =
  filter (all (`elem` s)) patInsts
  where
    patInsts = map (\a -> map (+ a) pat) s

-- Randomization based on Pitch spaces and patterns
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
        else choose patts g1 -- no nearby instances available

-- LESS complex randomization, but randomize duration
pGen2 :: [AbsPitch] -> [Dur] -> Int -> StdGen -> Music (AbsPitch, Volume)
pGen2 pitches durs thresh g0 =
  let (p, g1) = choose pitches g0
      (d, g2) = choose durs g1
      (v, g3) = randomR (0, 127) g2
      x =
        if v < thresh
          then rest d
          else note d (p, v)
   in x :+: pGen2 pitches durs thresh g3

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
    patterns = [[3, 0], [0, 5], [0, 3], [5, 0], [0, 7]] :: [Pattern]
    distThresh = 2 :: DistThresh
    root = 40 :: Root
    seed = mkStdGen 6 :: StdGen

pGenExample :: Music AbsPitch
pGenExample = line $ map (note $ fst duration) exampleSet
  where
    duration = choose [sn, sn, en, qn] (mkStdGen 500)

-- Play Examples
playPGen :: IO ()
playPGen = do
  play $ tempo 1 (instrument Vibraphone pGenExample)

playPGen2 :: IO ()
playPGen2 = do
  play $ tempo 2 (instrument Vibraphone melody :=: instrument Vibraphone melody2)
  where
    melody = pGen2 cMajorScale [hn, wn, hn, qn] 0 (mkStdGen 500)
    melody2 = pGen2 (map (+ 12) cMajorScale) [qn, sn, qn, en] 0 (mkStdGen 600)
