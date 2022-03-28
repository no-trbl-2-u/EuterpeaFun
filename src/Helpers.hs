module Helpers where

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

choose :: [a] -> SMGen -> (a, SMGen)
choose [] g = error "Nothing to Choose from!"
choose xs g =
  let (i, g') = randomR (0, 1000) g
   in (xs !! (i `mod` length xs), g')
