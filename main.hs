module Main where
import Data.List (sortBy, sort)
import Control.Monad
import System.Random
import Text.XHtml (shape, selected)

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
    isPrefixed x = value x >= 0


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
    dx = [x, x, x+1, x+1, x+1, x-1, x-1, x-1]
    dy = [y+1, y-1, y, y+1, y-1, y+1, y, y-1]


solve :: Board -> [Node]
solve board = backtrack (head $ sortPrefixedNodes board) [] missingValues (length missingValues) (getZeroNodes board) where
    missingValues = calculateMissingValues board


backtrack :: Node -> [Node] -> [Int] -> Int -> [Node] -> [Node]
backtrack node numbersPutInBoard missingValues totalMissingValues zeroNodes = do
    possiblePosition <- getPositionsOfAdjacentZeroNodes node zeroNodes
    guard $ null possiblePosition
    if length numbersPutInBoard == totalMissingValues then
        return node
    else do
        let newNode = Node (head missingValues) possiblePosition
        backtrack newNode (numbersPutInBoard ++ [newNode]) (tail missingValues) totalMissingValues (removeNodeByPosition zeroNodes possiblePosition)


generateCoordinates :: [[Integer]] -> Integer -> Integer -> [(Int, Int)]
generateCoordinates grid max min =
    [(x,y) | (x, rows) <- enumerate grid, (y,value) <- enumerate rows, value < max, value >= min ]
    where
        enumerate = zip [0..]

getElement :: [a] -> Int -> a
getElement list 1 = head list
getElement list n = getElement (tail list) (n-1)

fill :: Board -> (Int, Int) -> Board
fill board pos = newBoard where
    start = minNum board
    end = countZeros cells board
    newBoard = fillR board start pos end

fillR :: Board -> Int -> (Int, Int) -> Int -> Board
fillR board start (x,y) last
    | start == last = newBoard
    | cantContinue (x,y) (dx, dy) (cells board) = Empty
    | otherwise = maybeBoard
    where
        dx = [0, 0,  1, 1,  1, -1, -1, -1]
        dy = [1, -1, 0, 1, -1,  1,  0, -1]

        cellsList = replaceAt (x,y) start (cells board)
        newBoard = Board cellsList (minNum board) last

        newStart = start + 1
        maybeBoard = recursiveCall 0

        recursiveCall index
            | index == length dx = Empty

generateBoard :: Integer -> Integer -> Board
generateBoard a b = do
    seed <- newStdGen
    shape <- randomRIO(0,1)

    let boards = [squareBoard a b]
    let selected = getElement boards shape

    let blankCells = generateCoordinates (cells selected) 1 0
    randIndex <- randomRIO(0,length blankCells -1)
    let start = getElement blankCells randIndex

    --- Fill the blank board
    let board = fill selected start

    --- get the filled positions list
    let filledList = generateCoordinates(cells board) (maxNum board) 2
    let n = length filledList

    let list = map(`mod` n) randomList n seed
    let deleteList = sort (zip list filledList)

    let generatedBoard = removeNum board eliminationList (round (fromInteger (maxNum board)/2))
    -- printBoard generatedBoard
    return generatedBoard

sample :: Board
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

    