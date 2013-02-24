import System.Environment
import System.Random

import Parser

main = do
    arguments <- getArgs
    let statement = foldl (++) "" arguments
    case parse $ statement of
        Left err -> print err
        Right input -> do
            result <- ndn input
            putStrLn $ show result

ndn :: [ParseResult] -> IO Int
ndn ((Number numDice) : DieRoll : (Number dieSize) : _) =
        rollDice numDice dieSize
ndn ((Number left) : Add : (Number right) : _) = return $ left + right
ndn ((Number left) : Subtract : (Number right) : _) = return $ left - right
ndn ((Number left) : Multiply : (Number right) : _) = return $ left * right

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
