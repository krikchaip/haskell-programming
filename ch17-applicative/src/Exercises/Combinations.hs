module Exercises.Combinations where

import Control.Applicative (liftA3)

stops :: String
stops = "pbtdkg"

vowels :: String
vowels = "aeiou"

combos :: [a] -> [b] -> [c] -> [(a, b, c)]
combos = liftA3 (,,)

combos' :: [a] -> [b] -> [c] -> [(a, b, c)]
combos' xs ys zs = [(x, y, z) | x <- xs, y <- ys, z <- zs]
