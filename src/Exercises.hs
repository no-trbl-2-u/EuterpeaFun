module Exercises where

-- HSoM pg. 61-63

-- Type Aliases
type IntTuple = (Int, Int)

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
addPairsPointwise = foldr addPointwiseReducer (0, 0)
  where
    addPointwiseReducer :: IntTuple -> IntTuple -> IntTuple
    addPointwiseReducer (acc0, cur0) (acc1, cur1) = (acc0 + acc1, cur0 + cur1)

--  -- Example Sets
fns0 :: [Integer -> Integer]
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
