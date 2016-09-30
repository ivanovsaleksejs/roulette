module Roulette.Helpers where

import Data.Char hiding (isNumber)
import Data.Tuple.Utils

-- Helper functions
--
-- Check if argument is index in list of triples
inList :: Eq a => a -> [(a, b, c)] -> Bool
inList = any . (. fst3) . (==)

-- Split string by comma and convert to Int
toNumbers = map (\x -> read x :: Int) . splitBy ','

isNumber = all isDigit

-- Check if string is numbers separated by comma
checkNumbers = all isNumber . splitBy ','

-- Find index in list of triples and execute function on that triple
tripleLookup _ [] _ z = z
tripleLookup a (l:ls) f z
    | fst3 l == a = f l
    | otherwise   = tripleLookup a ls f z

-- Check if number count is 1, 3 or 6 and all numbers are in [1..36]
checkList l lengths = (length l') `elem` lengths && all (flip elem [0..36]) l'
    where l' = toNumbers l

-- Split string by delimiter
splitBy delimiter = foldr f [[]]
    where
        f c l@(x:xs)
            | c == delimiter = [] : l
            | otherwise      = (c:x) : xs

-- Non-contexted lookup with default value
myLookup a [] z = z
myLookup a (l:ls) z
    | fst l == a = snd l
    | otherwise  = myLookup a ls z
