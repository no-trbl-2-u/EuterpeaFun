{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use maximum" #-}
-- To recreate maxAbsPitch w/o eta reduction

{-# HLINT ignore "Use minimum" #-}
-- To recreate minAbsPitch w/o eta reduction
module Exercises where

import Data.List (sort)
import Euterpea

-- HSoM pg. 61-63

-- Type Aliases
type IntTuple = (Int, Int)

type PitchSpace = [AbsPitch]

type Step = Int

-- Exercise 3.3 (applyEach creates a list of results of funcN x)
applyEach :: [a -> b] -> a -> [b]
applyEach fns x = map ($ x) fns

-- Exercise 3.4 (applyAll func is funcN..(func2 $ func1 x))
applyAll :: [a -> a] -> a -> a
applyAll = foldr (.) id

-- Exercise 3.6 (Non-recursive length of [a])
listLength :: [a] -> Int
listLength = sum . map (const 1)

-- Exercise 3.7 (a. Double each number in a list)
doubleEach :: Num a => [a] -> [a]
doubleEach = map (* 2)

-- Exercise 3.7 (b. Pairs each element in a list with that number and one plus that number)
pairAndOne :: [Int] -> [IntTuple]
pairAndOne = map (\n -> (n, n + 1))

-- Exercise 3.7 (c. Adds together each pair of numbers in a list)
addEachPair' :: [IntTuple] -> [Int]
addEachPair' = map (uncurry (+))

-- Exercise 3.7 (d. Adds “pointwise” the elements of a list of pairs)
addPairsPointwise :: [IntTuple] -> IntTuple
addPairsPointwise = foldr reducer (0, 0)
  where
    reducer :: IntTuple -> IntTuple -> IntTuple
    reducer (a0, c0) (a1, c1) = (a0 + a1, c0 + c1)

-- Exercise 3.8 (Find max AbsPitch in [AbsPitch])
maxAbsPitch :: PitchSpace -> AbsPitch
maxAbsPitch [] = error "maxAbsPitch was given an empty array"
maxAbsPitch xs = last $ sort xs

minAbsPitch :: PitchSpace -> AbsPitch
minAbsPitch [] = error "minAbsPitch was given an empty array"
minAbsPitch xs = head $ sort xs

-- Exercise 3.9 (Create Chromatic scale asc/des/stay from 2 pitches)
chrom :: Pitch -> Pitch -> Music Pitch
chrom p0 p1
  | p0 == p1 = line [note qn p0] -- stagnant
  | p0 < p1 = line $ buildChrom p0 p1 -- ascending
  | otherwise = line $ reverse $ buildChrom p1 p0 -- descending
  where
    buildChrom :: Pitch -> Pitch -> [Music Pitch]
    buildChrom p0 p1 = map (note qn . pitch) [absPitch p0 .. absPitch p1]

-- Exercise 3.10 (Creates a scale from root and interval pattern)
majorSteps :: [Step]
majorSteps = [2, 2, 1, 2, 2, 2, 1]

mkScale :: Pitch -> [Step] -> Music Pitch
mkScale root [] = note qn root
mkScale root (interval : intervals) =
  note qn root :+: mkScale (trans interval root) intervals

-- Exercise 3.10 (Create a type for each mode)
data MajorScaleMode
  = Ionian'
  | Dorian'
  | Phrygian'
  | Lydian'
  | Mixolydian'
  | Aeolian'
  | Locrian'

genScale :: Pitch -> MajorScaleMode -> Music Pitch
genScale p mode =
  mkScale p (getByMode mode)
  where
    getByMode Ionian' = [2, 2, 1, 2, 2, 2, 1]
    getByMode Dorian' = [2, 1, 2, 2, 2, 1, 2]
    getByMode Phrygian' = [1, 2, 2, 2, 1, 2, 2]
    getByMode Lydian' = [2, 2, 2, 1, 2, 2, 1]
    getByMode Mixolydian' = [2, 2, 1, 2, 2, 1, 2]
    getByMode Aeolian' = [2, 1, 2, 2, 1, 2, 2]
    getByMode Locrian' = [1, 2, 2, 1, 2, 2, 2]

----------------- Example Sets ------------

fns0 :: [Int -> Int]
fns0 = [(+ 10), (+ 5), (+ 20)]

fns1 :: [String -> String]
fns1 = [(++ "!"), (++ "?"), (++ "!"), (++ "?")]

strs0 :: [String]
strs0 = ["Hello", "World"]

nums0 :: [Int]
nums0 = [0, 2 .. 20]

nums1 :: [IntTuple]
nums1 = pairAndOne nums0

nums2 :: [IntTuple]
nums2 = pairAndOne [1, 2, 3]

ps0 :: PitchSpace
ps0 = [40 .. 70]

pitch0 :: Pitch
pitch0 = (C, 4)

pitch1 :: Pitch
pitch1 = (C, 6)
