module Main where
import Data.List (sortBy, sort, unfoldr)
import Data.Set (Set)
import qualified Data.Set as Set
import System.Random
import Control.Monad

data Board = Board {
    cells :: [Node],
    minNum :: Int,
    maxNum :: Int
} | Empty deriving (Show, Eq)

main :: IO ()
main = print $ solve sample

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

getZeroNodes' :: [Node] -> [Node]
getZeroNodes' [] = []
getZeroNodes' cells = filter isZero cells where
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
solve board = backtrack board currentNode [] missingValues (length missingValues) (getZeroNodes board) where
    currentNode = head $ sortPrefixedNodes board
    missingValues = calculateMissingValues board



backtrack :: Board -> Node -> [Node] -> [Int] -> Int -> [Node] -> Board
backtrack board node numbersPutInBoard missingValues totalMissingValues zeroNodes
    | length numbersPutInBoard == totalMissingValues = getBoardIfSolved board node numbersPutInBoard
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


getBoardIfSolved :: Board -> Node -> [Node] -> Board
getBoardIfSolved board node numbersPutInBoard
    | null adjacent = Empty
    | otherwise = Board (getPrefixedNodes board ++ numbersPutInBoard) (minNum board) (maxNum board)
    where 
        adjacent = getSucessorNodeIfAdjacent node $ getSucessorNode board node

sample = Board
    [Node 0 (0,0), Node 33 (0,1), Node 35 (0,2),  Node 0 (0,3),  Node 0 (0,4),
    Node 0 (1,0),   Node 0 (1,1), Node 24 (1,2), Node 22(1,3),  Node 0 (1,4),
    Node 0 (2,0),   Node 0 (2,1),  Node 0 (2,2), Node 21 (2,3),  Node 0 (2,4), Node 0 (2,5),
    Node 0 (3,0),  Node 26 (3,1),  Node 0 (3,2), Node 13 (3,3), Node 40 (3,4), Node 11 (3,5),
    Node 27 (4,0),  Node 0 (4,1),  Node 0 (4,2),  Node 0 (4,3),  Node 9 (4,4),  Node 0 (4,5), Node 1 (4,6), -- 4
    Node 0 (5,2),  Node 0 (5,3), Node 18 (5,4),  Node 0 (5,5), Node 0 (5,6),
    Node 0 (6,4),  Node 7 (6,5), Node 0 (6,6), Node 0 (6,7),
    Node 5 (7,6), Node 0 (7,7), Node (-1) (0, 5)]
    1 40


squareBoard :: Int -> Int -> Board
squareBoard n m =
    Board {cells = [Node 0 ( i,  j) | i <- [1..n], j <- [1..n]], minNum = 1, maxNum = n*m}


generateCoordinates :: [Node] -> Int -> Int -> [(Int, Int)]
generateCoordinates nodes max min =
    [position node | (x, node) <- enumerate nodes,  value node < max, value node >= min ]
    where
        enumerate = zip [0..]


getElement :: [a] -> Int -> a
getElement list 1 = head list
getElement list n = getElement (tail list) (n-1)


nodeIndexer :: (Int, Int) -> [Node] -> Node
nodeIndexer (x, y) board =
    head (filter (\i -> position i /= (x,y)) board)

generateNodesWithZeros :: Int -> Int -> [Node]
generateNodesWithZeros rows columns = [Node 0 (x, y) | x <- [0..rows-1], y <- [0..columns-1]]


generateRandomPosition :: Int -> Int -> [(Int,Int)] -> IO (Int, Int) 
generateRandomPosition rows columns previousPoints = do
    rowPosition <- randomRIO (0, rows-1)
    columnPosition <- randomRIO (0, columns-1)
    let position = (rowPosition, columnPosition)
    if position `elem` previousPoints
        then generateRandomPosition rows columns previousPoints
        else return position


generateBoard :: Int -> Int -> Int -> Int -> IO Board
generateBoard rows columns minValue maxValue = do 
    minPosition <- generateRandomPosition rows columns [(-1,-1)]
    maxPosition <- generateRandomPosition rows columns [minPosition]
    let minNode = Node minValue minPosition
    let maxNode = Node maxValue maxPosition
    let zeroNodes = generateNodesWithZeros rows columns
    let zeroNodesWithoutMin = removeNodeByPosition zeroNodes $ position minNode
    let zeroNodesWithoutMax = removeNodeByPosition zeroNodesWithoutMin $ position maxNode
    let initialNodes = [minNode, maxNode] ++ zeroNodesWithoutMax
    solvedBoard <- getSolvedGenerated (Board initialNodes minValue maxValue)
    readyNodes <- removeRandomPositionFromSolvedBoard rows columns (cells solvedBoard) [minPosition, maxPosition] $ (length (cells solvedBoard) `div` 2) - 1
    return (Board readyNodes minValue maxValue)



getSolvedGenerated :: Board -> IO Board
getSolvedGenerated initialBoard = do
    let solvedBoard = solve initialBoard
    if solvedBoard == Empty 
        then getSolvedGenerated initialBoard
        else return solvedBoard


removeRandomPositionFromSolvedBoard :: Int -> Int -> [Node] -> [(Int,Int)] -> Int -> IO [Node]
removeRandomPositionFromSolvedBoard rows columns nodes removedPoints numberOfPointsToRemove = do 
    randomPoint <- generateRandomPosition rows columns removedPoints
    let newNodes = removeNodeByPosition nodes randomPoint
    let finalNodes = Node 0 randomPoint : newNodes
    if numberOfPointsToRemove == 0
        then return finalNodes
        else removeRandomPositionFromSolvedBoard rows columns finalNodes (randomPoint : removedPoints) (numberOfPointsToRemove-1)

getLength :: Int -> Int
getLength x 
    | x < 10 = 1
    | otherwise = getLength (x `div` 10) + 1 

generateString :: Int -> String
generateString count 
    | count == 0 = ""
    | count == 1 = " "
    | otherwise = do
        " " ++ generateString (count - 1)


formatVal :: Int -> Int -> String
formatVal val max = do
    if val < 0 then do
        let spaces = generateString 0
        spaces
    else do
        let len = getLength max
        let curLen = getLength val

        if curLen == len then do show val
        else do
            let spaces = generateString (len - curLen)
            spaces ++ show val


printNodes :: [Node] -> Int -> Int -> IO ()
printNodes nodes idx max
    | idx < length nodes = do
        let node = nodes !! idx
        -- get second component of a tuple
        let (x, y) = position node
        if y == 0 then do
            putStr "\n"
            else do
                putStr " "
        let val = (value node)
        let newVal = formatVal val max
        putStr newVal
        printNodes nodes (idx+1) max
    | otherwise = do
        putStr " "

printBoard :: Board -> IO ()
printBoard board = do
    let boardToPrint = board
    if boardToPrint == Empty then print Empty
    else do
        let sortedNodesByPosition = sortBy (\x y -> compare (position x) (position y)) (cells boardToPrint)
        let max = maxNum board
        printNodes sortedNodesByPosition 0 max
        putStr "\n"













