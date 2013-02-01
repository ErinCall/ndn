import System.Environment

main = do
    expression <- getArgs
    putStrLn $ show $ ndn expression

ndn :: [String] -> Int
ndn (numDice : _ : dieSize : _) = rollDice (read numDice) (read dieSize)

rollDice :: Int -> Int -> Int
rollDice 1 dieSize = rollDie dieSize
rollDice n dieSize = (rollDie dieSize) + (rollDice (n - 1) dieSize)

rollDie :: Int -> Int
rollDie dieSize = dieSize
