module Roulette.Helpers where

import Data.Char hiding (isNumber)
import Data.List
import Data.Tuple.Utils

-- Helpers

-- Check if all symbols in string are digits
isNumber n = n /= "" && all isDigit n

-- Check if string is numbers separated by comma
checkNumbers = all isNumber . splitBy ','

-- Split string by comma and convert to Int
toNumbers = nub . map (\x -> read x :: Int) . splitBy ','

-- Split string by delimiter
splitBy delimiter = foldr checkDelimiter [[]]
    where
        checkDelimiter c l@(x:xs)
            | c == delimiter = [] : l
            | otherwise      = (c:x) : xs

-- Check if number count is within allowed lengths and all numbers are in [0..36]
checkList l lengths = (length parsedList) `elem` lengths && all (`elem` [0..36]) parsedList
    where parsedList = toNumbers l

              -- Check if argument is index in list of triples
inList :: Eq a => a -> [(a, b, c)] -> Bool
inList index = any (\ x -> index == fst3 x)

-- Find index in list of triples and execute function on that triple
tripleLookup _ [] _ def = def
tripleLookup index (l:ls) fn def
    | fst3 l == index = fn l
    | otherwise       = tripleLookup index ls fn def

colorLine color line = "\x1b[" ++ color ++ "m" ++ line ++ "\x1b[0m"
[redLine, greenLine, pinkLine] = map colorLine ["31", "32", "35"]
