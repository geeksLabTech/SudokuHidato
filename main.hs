module Main where
import Data.List (sortBy, sort)
import Control.Monad


main :: IO ()
main = putStrLn "Hello, Haskell!"

data Board = Board {
    cells :: [Node],
    minNum :: Int,
    maxNum :: Int
} | Empty deriving (Show, Eq)


data Node = Node {
    value ::  Int, 
    position :: (Int, Int)
} deriving (Show, Eq, Ord)


getPrefixedNodes :: Board -> [Node]
getPrefixedNodes Empty = []
getPrefixedNodes (Board cells _ _) = filter isPrefixed cells where
    isPrefixed x = value x > 0


sortPrefixedNodes :: Board -> [Node]
sortPrefixedNodes Empty = []
sortPrefixedNodes board = sortBy compareByValue $ getPrefixedNodes board where
    compareByValue x y = compare (value x) (value y)


calculateMissingValues :: Board -> [Int]
calculateMissingValues Empty = []
calculateMissingValues board = [x | x <- [minNum board .. maxNum board], notElem x $ map value $ getPrefixedNodes board]


getZeroNodes :: Board -> [Node]
getZeroNodes Empty = []
getZeroNodes (Board cells _ _) = filter isZero cells where
    isZero x = value x == 0


-- removeNodeInBoard :: [(Int, Int)] -> [(Int, Int)]
removeNodeByPosition :: [Node] -> (Int,Int) -> [Node]
removeNodeByPosition [] _ = []
removeNodeByPosition nodes pos = filter (\x -> position x /= pos) nodes
   


isSolved :: [(Int, Int)] -> [Int] -> Bool
isSolved numbersPutInBoard missingValues = length numbersPutInBoard == length missingValues 


getPositionsOfAdjacentZeroNodes :: Node -> [Node] -> [(Int, Int)]
getPositionsOfAdjacentZeroNodes node zeroNodes = filter isNodeZero [(x, y) | x <- dx, y <- dy] where
    isNodeZero (x, y) = (x, y) `elem` map position zeroNodes
    (x, y) = position node
    dx = [x,   x,  x+1, x+1, x+1, x-1, x-1, x-1]
    dy = [y+1, y-1, y,  y+1, y-1, y+1,  y,  y-1]


solve :: Board -> [Node]
solve board = backtrack (head $ sortPrefixedNodes board) [] missingValues (length missingValues) (getZeroNodes board) where
    missingValues = calculateMissingValues board


backtrack :: Node -> [Node] -> [Int] -> Int -> [Node] -> [Node]
backtrack node numbersPutInBoard missingValues totalMissingValues zeroNodes = do
    posiblePosition <- getPositionsOfAdjacentZeroNodes node zeroNodes
    guard $ null posiblePosition
    if length numbersPutInBoard == totalMissingValues then
        return node
    else do
        let newNode = Node (head missingValues) posiblePosition
        backtrack newNode (numbersPutInBoard ++ [newNode]) (tail missingValues) totalMissingValues (removeNodeByPosition zeroNodes posiblePosition)


sample = Board  
    [Node 0 (0,0), Node 33 (0,1), Node 35 (0,2),  Node 0 (0,3),  Node 0 (0,4),          
    Node 0 (1,0),   Node 0 (1,1), Node 24 (1,2), Node 22(1,3),  Node 0 (1,4),        
    Node 0 (2,0),   Node 0 (2,1),  Node 0 (2,2), Node 21 (2,3),  Node 0 (2,4), Node 0 (2,5),     
    Node 0 (3,0),  Node 26 (3,1),  Node 0 (3,2), Node 13 (3,3), Node 40 (3,4), Node 11 (3,5),  
    Node 27 (4,0),  Node 0 (4,1),  Node 0 (4,2),  Node 0 (4,3),  Node 9 (4,4),  Node 0 (4,5), Node 1 (4,6), -- 4
    Node 0 (5,2),  Node 0 (5,3), Node 18 (5,4),  Node 0 (5,5), Node 0 (5,6), 
    Node 0 (6,4),  Node 7 (6,5), Node 0 (6,6), Node 0 (6,7), 
    Node 5 (7,6), Node 0 (7,7)] 
    1 40

    