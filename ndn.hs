import System.Environment
import System.Random

main = do
    expression <- getArgs
    gen <- getStdGen
    putStrLn $ show $ ndn expression gen

ndn :: (RandomGen g) => [String] -> g -> Int
ndn (numDice : _ : dieSize : _) = rollDice (read numDice) (read dieSize)

rollDice :: (RandomGen g) => Int -> Int -> g -> Int
rollDice 1 dieSize g = rollDie dieSize g
rollDice n dieSize g = (rollDie dieSize g) + (rollDice (n - 1) dieSize g)

rollDie :: (RandomGen g) => Int -> g -> Int
rollDie dieSize = fst . randomR (1, dieSize)
