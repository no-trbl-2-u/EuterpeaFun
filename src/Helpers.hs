module Helpers (findInsts, choose) where

import Data.List (nub)
import Euterpea (AbsPitch)
import System.Random
import System.Random.SplitMix

-- Pattern Mapping helpers
findInsts :: [AbsPitch] -> [AbsPitch] -> [[AbsPitch]]
findInsts s pat =
  filter (all (`elem` s)) patInsts
  where
    patInsts = map (\a -> map (+ a) pat) s

-- Randomness helpers
randomize :: SMGen -> [a] -> [a]
randomize sg rs =
  let n = length rs
      plist = take n (nub (randomRs (0, n -1) sg))
   in map (rs !!) plist

-- | Takes a 'list' and an 'SMGen' and returns a tuple of the
-- randomly selected value and the nextGen func via a
-- randomly created index value to use within the xs
choose :: [a] -> SMGen -> (a, SMGen)
choose [] gen = error "Nothing to Choose from!"
choose xs gen =
  let (randIdx, genNext') = randomR (0, 1000) gen
      safeIdx = randIdx `mod` length xs :: Int -- index within range of list
   in (xs !! safeIdx, genNext')
