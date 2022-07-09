module Main where
import Data.List (sortBy, sort, unfoldr)
import Data.Set (Set)
import qualified Data.Set as Set
import System.Random
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
    isPrefixed x = value x >= 0


sortPrefixedNodes :: Board -> [Node]
sortPrefixedNodes Empty = []
sortPrefixedNodes board = sortBy compareByValue $ getPrefixedNodes board where
    compareByValue x y = compare (value x) (value y)


calculateMissingValues :: Board -> [Int]
calculateMissingValues Empty = []
calculateMissingValues board = [ x | x <- [minNum board .. maxNum board], notElem x $ map (value) $ getPrefixedNodes board]


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
    head (filter (\i -> position i /= (x,y)) (board))

fill :: Board -> (Int, Int) -> Board
fill board pos = newBoard where
    start = minNum board
    end = length (getZeroNodes board)
    newBoard = fillRecursive board start pos end

fillRecursive :: Board -> Int -> (Int, Int) -> Int -> Board
fillRecursive board start (x,y) last
    | start == last = newBoard
    | not (null (getPositionsOfAdjacentZeroNodes (head (filter (\i -> position i /= (x,y)) (cells board))) ))= Empty
    | otherwise = maybeBoard
    where
        dx = [0, 0,  1, 1,  1, -1, -1, -1]
        dy = [1, -1, 0, 1, -1,  1,  0, -1]

        -- list resulting to add one Node to cellsList
        cellsList = Node ( start) (x, y) : removeNodeByPosition (cells board) (x,y)
        newBoard = Board cellsList (minNum board) last

        newStart = start + 1
        maybeBoard = recursiveCall 0

        recursiveCall index
            | index == length dx = Empty
            | not (inRange (newX, newY) cellsList) || value (nodeIndexer (newX, newY) cellsList)  /= 0 = recursiveCall (index+1)
            | canDisconnect (cells newBoard) x y && connected(cells newBoard) newX newY < last - start = recursiveCall(index+1)
            | solution /= Empty = solution
            | otherwise = recursiveCall (index + 1)
            where
                (newX, newY) = (x + getElement dx index, y + getElement dy index )
                solution = fillRecursive newBoard newStart (newX, newY) last

connected :: [Node] -> Int -> Int -> Int
connected board x y 
    | nodeIndexer (x, y) board /= 0 = 0
    | otherwise = findFinalResult (Set.fromList[(x,y)])
    where
        findFinalResult :: Set (Int, Int) -> Int
        findFinalResult zeros 
            | zeros == nZeros = Set.size nZeros
            | otherwise = findFinalResult nZeros
            where
                nZeros = neighbors zeros
                neighbors :: Set (Int, Int) -> Set (Int, Int)
                neighbors zeros
                    | Set.size zeros == 0 = Set.empty
                    | otherwise = Set.union nZeros (neighbors popZeros)
                    where
                        dx = [0, 0, 0,  1, 1,  1, -1, -1, -1]
                        dy = [0, 1, -1, 0, 1, -1,  1,  0, -1]
                        (x, y) = Set.findMin zeros
                        popZeros = Set.deleteMin zeros
                        nZeros = Set.fromList [(x+getElement dx i,y+ getElement dy i) | i <- [0..(length dx)-1], inRange (x+getElement dx i, y+getElement dy i) board, value (nodeIndexer ((x+getElement dx i), (y+getElement dy i)) board) == 0]


canDisconnect :: [Node] -> Int -> Int -> Bool
canDisconnect board x y
    | length zeros <= 1 = False
    | inRange(up, y) board && inRange(x, right) board && inRange(up, right) board && intercept up right = True
    | inRange(up, y) board && inRange(x, left) board && inRange(up, left) board && intercept up left = True
    | inRange(down, y) board && inRange(x, right) board && inRange(down, right) board && intercept down right = True
    | inRange(down, y) board && inRange(x, left) board && inRange(down, left) board && intercept down left = True
    | inRange(x, right) board && inRange(x, left) board && separatesC = True
    | inRange(down, y) board && inRange(up, y) board && separatesR = True
    | otherwise = False
    where
        up = x-1
        down = x+1
        left = y-1
        right = y+1
        intercept nX nY = value (nodeIndexer(x, nY) board) /= 0 && value (nodeIndexer(nX, y) board) /= 0 && value (nodeIndexer(nX, nY) board) /= 0
        dx = [0, 0, 0,  1, 1,  1, -1, -1, -1]
        dy = [0, 1, -1, 0, 1, -1,  1,  0, -1]
        zeros = filter (\x -> x==0) [nodeIndexer (x + getElement dx i, y+getElement dy i ) board | i <- [0..length dx-1], inRange (x+getElement dx i,y+getElement dy i) board]

        separatesC = ((not (inRange (up,y) board)) || nodeIndexer (up,y) board /= 0)  &&
                     ((not (inRange (down,y) board)) || nodeIndexer (down,y) board /= 0) &&
                     ((inRange (up,left) board && nodeIndexer (up,left) board == 0) ||
                      (inRange (down,left) board && nodeIndexer (down,left) board == 0) || nodeIndexer (x,left) board == 0) &&
                     ((inRange (up,right) board && nodeIndexer (up,right) board == 0) ||
                      (inRange (down,right) board && nodeIndexer (down,right) board == 0) || nodeIndexer (x,right) board == 0)
        separatesR = ((not (inRange (x,left) board)) || nodeIndexer (x,left) board /= 0) &&
                     ((not (inRange (x,right) board)) ||  nodeIndexer (x,right) board /= 0) &&
                    ((inRange (up,left) board  && nodeIndexer (up,left) board == 0 ) ||
                     (inRange (up,right) board && nodeIndexer (up,right) board == 0) || nodeIndexer (up,y) board == 0) &&
                    ((inRange (down, left) board && nodeIndexer (down,left) board == 0 ) ||
                     (inRange (down, right) board && nodeIndexer (down,right) board == 0) || nodeIndexer (down,y) board == 0)
 

inRange :: (Int, Int) -> [Node] -> Bool
inRange (x, y) board =
    x >= 0 && x < length board && y >= 0 && y < length board

removeNum :: Board -> [(Int, (Int, Int))] -> Int -> Board
removeNum board list n
    | n == 0 = board
    | otherwise = removeNum newBoard rest (n-1)
    where
        (init:rest) = list
        (index,pos) = init
        newCells = Node 0 pos : removeNodeByPosition (cells board) pos
        newBoard = Board newCells (minNum board) (maxNum board)

randomList :: Int -> StdGen -> [Int]
randomList n = take n . unfoldr (Just . random)

generateBoard :: Int -> Int -> Board
generateBoard a b = do
    nSeed <- newStdGen
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

    -- let list = map(`mod` n) randomList(n,nSeed)
    let list = map(\x -> (mod x n)) (randomList n nSeed)
    let deleteList = sort (zip list filledList)

    let generatedBoard = removeNum board deleteList (round ( (maxNum board)/2))
    -- printBoard generatedBoard
    return generatedBoard

