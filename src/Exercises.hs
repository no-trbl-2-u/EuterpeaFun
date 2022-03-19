module Exercises where

-- Exercise 3.3
applyEach :: [a -> b] -> a -> [b]
applyEach fns x = map ($ x) fns

-- Exercise 3.4
applyAll :: [a -> a] -> a -> a
applyAll = foldr (.) id

--  -- Example Sets
fns0 :: [Integer -> Integer]
fns0 = [(+ 10), (+ 5), (+ 20)]

fns1 :: [String -> String]
fns1 = [(++ "!"), (++ "?"), (++ "!"), (++ "?")]
