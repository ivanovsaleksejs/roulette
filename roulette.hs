import Roulette.Helpers
import Roulette.Game

main = gameStart

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
                    putStrLn $ redLine "Wrong input!"
                    gameStart
