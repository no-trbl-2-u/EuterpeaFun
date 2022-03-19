module Exercises where

-- HSoM pg. 61-63

-- Exercise 3.3
applyEach :: [a -> b] -> a -> [b]
applyEach fns x = map ($ x) fns

-- Exercise 3.4
applyAll :: [a -> a] -> a -> a
applyAll = foldr (.) id

-- TODO: Exercises 3.5+

--  -- Example Sets
fns0 :: [Integer -> Integer]
fns0 = [(+ 10), (+ 5), (+ 20)]

fns1 :: [String -> String]
fns1 = [(++ "!"), (++ "?"), (++ "!"), (++ "?")]
