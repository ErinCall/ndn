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
rollDice n dieSize g = let (roll, newGen) = rollDie' dieSize g
                       in roll + (rollDice (n - 1) dieSize newGen)

rollDie :: (RandomGen g) => Int -> g -> Int
rollDie size = fst . rollDie' size

rollDie' :: (RandomGen g) => Int -> g -> (Int, g)
rollDie' dieSize = randomR (1, dieSize)
