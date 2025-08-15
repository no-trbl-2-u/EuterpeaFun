module Helpers (
  findInsts,
  choose,
  chooseMany
) where

import           System.Random
import           System.Random.SplitMix

type AbsPitch = Int

-- Pattern Mapping helpers
findInsts :: [AbsPitch] -> [AbsPitch] -> [[AbsPitch]]
findInsts s pat =
  filter (all (`elem` s)) patInsts
  where
    patInsts = map (\a -> map (+ a) pat) s

-- | Takes a 'list' and an 'SMGen' and returns a tuple of the
-- randomly selected value and the nextGen func via a
-- randomly created index value to use within the xs
choose :: RandomGen g => [a] -> g -> (a, g)
choose [] _ = error "Nothing to Choose from!"
choose xs gen =
  let (randIdx, genNext') = randomR (0, length xs - 1) gen
   in (xs !! randIdx, genNext')

-- | Choose multiple items from a list randomly
-- Returns a list of n random items and the updated generator
chooseMany :: RandomGen g => Int -> [a] -> g -> ([a], g)
chooseMany n xs gen
  | n <= 0 = ([], gen)
  | null xs = ([], gen)
  | otherwise = go n xs gen []
  where
    go 0 _ g acc = (reverse acc, g)  -- Base case: we've chosen enough items, reverse to maintain order
    go count items g acc =
      let (chosen, g') = choose items g
      in go (count - 1) items g' (chosen : acc)
