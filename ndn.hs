import System.Environment
import System.Random

import Parser

main = do
    arguments <- getArgs
    let statement = foldl (++) "" arguments
    case parse $ statement of
        Left err -> print err
        Right input -> do
            result <- evaluate input
            putStrLn $ show result

evaluate :: Expression -> IO Int
evaluate (Number n) = return n
evaluate (Add left right) = arithmetic left right (+)
evaluate (Multiply left right) = arithmetic left right (*)
evaluate (Subtract left right) = arithmetic left right (-)
evaluate (DieRoll left right) = do
    numDice <- evaluate left
    dieSize <- evaluate right
    rollDice numDice dieSize

arithmetic left right op = do
    l <- evaluate left
    r <- evaluate right
    return $ op l r

rollDice :: Int -> Int -> IO Int
rollDice 1 dieSize = rollDie dieSize
rollDice n dieSize = do
    thisRoll <- rollDie dieSize
    otherRolls <- rollDice (n - 1) dieSize
    return $ thisRoll + otherRolls

rollDie :: Int -> IO Int
rollDie dieSize = do
    (roll, _) <- rollDie' dieSize
    return roll
    where rollDie' dieSize = do
            gen <- newStdGen
            return $ randomR (1, dieSize) gen
