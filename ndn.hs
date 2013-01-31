import System.Environment
import System.Random

main = do
    expression <- getArgs
    result <- ndn expression
    putStrLn $ show result

ndn :: [String] -> IO Int
ndn (numDice : "d" : dieSize : _) = rollDice (read numDice) (read dieSize)

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

rollDie' :: Int -> IO (Int, StdGen)
rollDie' dieSize = do
    gen <- newStdGen
    return $ randomR (1, dieSize) gen
