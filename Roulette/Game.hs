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

numberBets = [1,3,6]

end s = putStrLn $ "Game ended: " ++ s

quit s = any (`isPrefixOf` s) [":q", "q", "exit"]

game betSize money
    | money <= 0      = end $ redLine "You lost!"
    | betSize > money = do
        putStrLn $ pinkLine $ "Money too low, bet value changed to " ++ (show money) ++ "!"
        game money money
    | otherwise       = do
        putStrLn $ pinkLine $ "Money: " ++ (show money) ++ ". Bet value: " ++ (show betSize)
        putStrLn "Enter your bet (to change amount of bet enter 'change <number>')"
        input <- getLine
        processInput betSize input money


processInput betSize input money
    | quit input             = end "quit."
    | change                 = changeValue
    | simpleBet || numberBet = generateNumber betSize money winners quotient multiplier
    | otherwise              = wrongInput
    where
        change    = isPrefixOf "change" input

        changeValue
            | validValue = game newValNum money
            | otherwise  = wrongInput
            where
                newVal     = dropWhile (== ' ') $ dropWhile (/= ' ') input
                newValNum  = read newVal :: Int
                validValue = isNumber newVal && newValNum >= 1 && newValNum <= money

        wrongInput = do
            putStrLn $ redLine "Wrong input!"
            game betSize money

        simpleBet = inList input bets
        numberBet = checkNumbers input && checkList input numberBets

        numbers  = toNumbers input
        winners
            | simpleBet = tripleLookup input bets snd3 []
            | numberBet = numbers

        quotient
            | simpleBet = tripleLookup input bets thd3 0
            | otherwise = 36

        multiplier
            | simpleBet = 1
            | otherwise = length numbers


generateNumber betSize money winners quotient multiplier = do
    gen <- fmap (`mod` 37) randomIO
    putStr $ "Number won: " ++ (show gen) ++ " "

    if gen == 0
        then putStrLn ""
        else do
            if elem gen reds then putStr (redLine "red") else putStr "black"
            if odd gen then putStrLn " odd" else putStrLn " even"

    if elem gen winners
        then do
            let w = (quotient - multiplier) * betSize
            putStrLn $ greenLine $ "You won " ++ (show w)
            game betSize $ money + w
        else do
            let l = betSize * multiplier
            putStrLn $ redLine "You lost"
            game betSize $ money - l
