import System.Environment
import System.Random

import Parser

main = do
    expression <- getArgs
    case parse $ unwords expression of
        Left err -> print err
        Right input -> do
            result <- ndn input
            putStrLn $ show result

ndn :: [ParseResult] -> IO Int
ndn ((Number numDice) : DieRoll : (Number dieSize) : _) =
        rollDice (read numDice) (read dieSize)

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
