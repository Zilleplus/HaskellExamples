module Queen where

import Data.List
import System.Random

data Position = Position {x :: Int
                         ,y :: Int
                         } deriving(Show,Eq)

createQueen :: Int -> Int -> Queen
createQueen x y = Queen{position=Position{x=x,y=y}}

data Queen = Queen  {position :: Position
                    } deriving(Show,Eq)

data Move = Move {startPosition :: Position
                 ,endPosition :: Position
                 } deriving(Show,Eq)

evaluateCost :: [Queen] -> Int
evaluateCost queens = 
    conflicts grouperX (x . position)
    + conflicts grouperY (y . position)
    + conflicts grouperDiagonalUp xyDiff
    + conflicts grouperDiagonalDown xySum

    where
        xyDiff q = (x . position) q -  (y . position) q
        xySum q = (x . position) q +  (y . position) q
        conflicts grouper sorter = 
            ((foldl (+) 0).
            (filter (> 1)) . 
            (map length) . 
            groupBy grouper
            ) (sortOn sorter queens)
        grouperDiagonal a b =  abs((x . position) a - (x . position) b) 
            ==  abs((y . position) a - (y . position) b)
        
        grouperDiagonalUp a b = (xyDiff a) == (xyDiff b) 
        grouperDiagonalDown a b = (xySum a) == (xySum b) 
        grouperX a b = (x . position) a == (x . position) b 
        grouperY a b =  (y . position) a == (y . position) b 

-- assume move is not on top of existing queen otherwise they stick together
applyMove :: [Queen] -> Move -> [Queen]
applyMove queens move =
    do {q <- queens;
        if (position q == (startPosition move))
            then return Queen{position= endPosition move}
            else return q}

evaluateCostWithMove :: [Queen] -> Move -> Int
evaluateCostWithMove queens move = 
    evaluateCost (applyMove queens move)
        
generateMovesQueen :: [Queen] -> Queen -> [Move]
generateMovesQueen queens queen =
    [Move{startPosition=position queen,endPosition=p} | 
        x<-[0..boardSize-1], 
        y<-[0..boardSize-1],
        let p = Position{x=x,y=y},
        not $ any (==p) usedQueenPositions ]
    where 
        boardSize = 8
        usedQueenPositions = map position queens 

--either we find viable move and we continue, or we dont and we are converged
steepestDescent :: [Queen] -> [Queen]
steepestDescent queens = 
    case firstViableMove of 
        Just acceptedMove -> steepestDescent (applyMove queens acceptedMove)
        _ -> queens -- converged
    where 
        possibleMoves =  queens >>= (generateMovesQueen queens)
        acceptCondition  move =  (evaluateCostWithMove queens move) < (evaluateCost queens)
        firstViableMove = find acceptCondition possibleMoves

simulatedAnnealing :: StdGen -> [Queen] ->  [Queen]
simulatedAnnealing  generator queens = 
    case (firstViableMove generator) of
        Just m ->  applyMove queens m
        _ -> queens
        where 
            firstViableMove :: StdGen -> Maybe Move
            firstViableMove gen = (find acceptCondition (movesWithRandom gen)) >>= (\(a,b) -> Just a)
            
            acceptCondition :: (Move,Double) -> Bool
            acceptCondition (move,rand) = rand > 0.5

            possibleMoves =  queens >>= (generateMovesQueen queens)
            movesWithRandom g = zip possibleMoves (randomRs (0.0,1.0) g )