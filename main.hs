import Data.List (sortBy, sort, find)
import Control.Monad
import System.Random


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


getNodeInPosition :: Board -> (Int, Int) -> [Node]
getNodeInPosition (Board cells _ _) (x, y) = filter (\node -> position node == (x, y)) cells


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


removeNodeByPosition :: [Node] -> (Int,Int) -> [Node]
removeNodeByPosition [] _ = []
removeNodeByPosition nodes pos = filter (\x -> position x /= pos) nodes


isSolved :: [(Int, Int)] -> [Int] -> Bool
isSolved numbersPutInBoard missingValues = length numbersPutInBoard == length missingValues


getPositionsOfAdjacentZeroNodes :: Node -> [Node] -> [(Int, Int)]
getPositionsOfAdjacentZeroNodes node zeroNodes = filter isNodeZero $ map (\(x,y) -> (x + fst coordinate, y + snd coordinate)) directions where
    isNodeZero (x, y) = (x, y) `elem` map position zeroNodes
    coordinate = position node
    directions = [(1,0), (0,1), (1,1), (1,-1), (-1,1), (-1,0), (0,-1), (-1,-1)]


getSucessorNode :: Board -> Node -> [Node]
getSucessorNode (Board cells _ _) node = filter isSucessor cells where
    isSucessor x = value x == 1 + value node


getSucessorNodeIfAdjacent :: Node -> [Node] -> [Node]
getSucessorNodeIfAdjacent node target
    | null target = []
    | position (head target) `elem` adjacents = [head target]
    | otherwise = []
    where
        adjacents = map (\(x,y) -> (x + fst coordinate, y + snd coordinate)) directions where
            coordinate = position node
            directions = [(1,0), (0,1), (1,1), (1,-1), (-1,1), (-1,0), (0,-1), (-1,-1)]



solve :: Board -> Board
solve board = backtrack board numbersPutInBoard [] missingValues (length missingValues) (getZeroNodes board) where
    numbersPutInBoard = head $ sortPrefixedNodes board
    missingValues = calculateMissingValues board



backtrack :: Board -> Node -> [Node] -> [Int] -> Int -> [Node] -> Board
backtrack board node numbersPutInBoard missingValues totalMissingValues zeroNodes
    | length numbersPutInBoard == totalMissingValues = Board numbersPutInBoard (minNum board) (maxNum board)
    | otherwise = maybeBoard
        where
        positions = getPositionsOfAdjacentZeroNodes node zeroNodes
        maybeBoard = recursiveCall positions
        recursiveCall positionsToCheck
            | not $ null sucessorNode = callInSucessorNode $ head sucessorNode
            | null positionsToCheck = Empty
            | solution /= Empty = solution
            | otherwise = recursiveCall $ tail positionsToCheck
            where
            newNode = Node (head missingValues) (head positionsToCheck)
            solution = backtrack board newNode (numbersPutInBoard ++ [newNode]) (tail missingValues) totalMissingValues (removeNodeByPosition zeroNodes $ head positionsToCheck)
            sucessorNode = getSucessorNodeIfAdjacent node $ getSucessorNode board node
            callInSucessorNode sucessor = backtrack board sucessor numbersPutInBoard missingValues totalMissingValues zeroNodes


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

    