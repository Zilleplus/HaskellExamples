module Main where

import Queen
import System.Random

main :: IO ()
main = do
    print "End"
    -- start with all the queens on the diagonal
    let queens = [ createQueen i 0  |i<- [0 .. 7]]
        
    -- do simple steepest descent
    let solution = (steepestDescent queens)

    -- define a start position
    print "Pure descent converges too:"
    print solution 
    print (evaluateCost solution)

    let seed = 1
    let randomGenerator = mkStdGen seed 
    let solutionSA = (simulatedAnnealing randomGenerator solution)
    
    print "SA converges too:"
    print solutionSA 
    print (evaluateCost solutionSA)