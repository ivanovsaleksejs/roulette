import Data.Char hiding (isNumber)

import Roulette.Helpers
import Roulette.Game

-- Main cycle
gameStart = do
    putStrLn "Enter amount"
    m <- getLine
    if quit m
        then putStrLn "Bye!"
        else do
            if isNumber m
                then game 1 $ read m
                else do
                    putStrLn $ "\x1b[31m" ++ "Wrong input!" ++ "\x1b[0m"
                    gameStart

main = gameStart
