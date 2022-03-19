module Exercises where

-- Currying exercises
applyEach :: [a -> b] -> a -> [b]
applyEach fns x = map ($ x) fns

applyAll :: [a -> a] -> a -> a
applyAll = foldr (.) id

--  -- Examples
fns0 :: [Integer -> Integer]
fns0 = [(+ 10), (+ 5), (+ 20)]

fns1 :: [String -> String]
fns1 = [(++ "!"), (++ "?"), (++ "!"), (++ "?")]
