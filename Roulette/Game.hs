module Roulette.Game where

import Data.List
import Data.Tuple.Utils
import System.Random

import Roulette.Helpers

-- Bets
reds   = [1,3,5,7,9,12,14,16,18,19,21,23,25,27,30,32,34,36]
blacks = [1..36] \\ reds

bets = [
        ("odd",    [1,3..35],  2),
        ("even",   [2, 4..36], 2),
        ("red",    reds,       2),
        ("black",  blacks,     2),
        ("1..18",  [1..18],    2),
        ("19..36", [19..36],   2),
        ("1..12",  [1..12],    3),
        ("13..24", [13..24],   3),
        ("25..36", [25..36],   3)
    ]

val = [
        (1, 36),
        (3, 12),
        (6, 6)
    ]

-- Game functions
end s = putStrLn $ "Game ended: " ++ s

quit s = any (flip isPrefixOf s) [":q", "q"]

game v m
    | m <= 0    = end $ "\x1b[31m" ++ "You lost!" ++ "\x1b[0m"
    | otherwise = nextBet v m

nextBet v m = do
    if v > m
        then do
            putStrLn $ "\x1b[35m" ++ "Money too low, bet value changed to " ++ (show m) ++ "!" ++ "\x1b[0m"
            nextBet m m
        else do
            putStrLn $ "\x1b[35m" ++ "Money: " ++ (show m) ++ ". Bet value:" ++ (show v) ++ "\x1b[0m"
            putStrLn "Enter your bet (to change amount of bet enter change <number>)"
            b <- getLine
            processGame v b m

processGame v b m
    | quit b                                      = end "quit."
    | b == ""                                     = wrong
    | isPrefixOf "change" b                       = changeValue
    | inl                                         = generateNumber v x m $ tripleLookup b bets snd3 []
    | checkNumbers b && checkList b (map fst val) = generateNumber v x m s
    | otherwise                                   = wrong
    where
        change = isPrefixOf "change" b
        inl    = inList b bets
        s      = toNumbers b

        wrong = do
            putStrLn $ "\x1b[31m" ++ "Wrong bet!" ++ "\x1b[0m"
            nextBet v m

        x
            | inl       = tripleLookup b bets thd3 0
            | otherwise = myLookup (length s) val 0

        changeValue
            | validValue = nextBet newVal' m
            | otherwise  = wrong
            where
                newVal     = drop 1 $ dropWhile (/= ' ') b
                newVal'    = read newVal :: Int
                validValue = isNumber newVal && newVal' >= 1 && newVal' <= m

generateNumber v b m l = do
    w <- fmap (flip mod 37) randomIO
    putStr $ "Number won: " ++ (show w) ++ " "

    if w == 0
        then putStrLn ""
        else do
            if elem w reds then putStr ("\x1b[31m" ++ "red" ++ "\x1b[0m") else putStr "black"
            if odd w then putStrLn " odd" else putStrLn " even"

    if elem w l
        then do
            putStrLn $ "\x1b[32m" ++ "You won" ++ "\x1b[0m"
            game v $ m + (b - 1) * v
        else do
            putStrLn $ "\x1b[31m" ++ "You lost" ++ "\x1b[0m"
            game v $ m - v
