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

-- fill :: Board -> (Int, Int) -> Board
-- fill board pos = newBoard where
--     start = minNum board
--     end = length (getZeroNodes board)
--     newBoard = fillRecursive board start pos end

-- fillRecursive :: Board -> Int -> (Int, Int) -> Int -> Board
-- fillRecursive board start (x,y) last
--     | start == last = newBoard
--     | not (null (getPositionsOfAdjacentZeroNodes (head (filter (\i -> position i /= (x,y)) (cells board))) ))= Empty
--     | otherwise = maybeBoard
--     where
--         dx = [0, 0,  1, 1,  1, -1, -1, -1]
--         dy = [1, -1, 0, 1, -1,  1,  0, -1]

--         -- list resulting to add one Node to cellsList
--         cellsList = Node ( start) (x, y) : removeNodeByPosition (cells board) (x,y)
--         newBoard = Board cellsList (minNum board) last

--         newStart = start + 1
--         maybeBoard = recursiveCall 0

--         recursiveCall index
--             | index == length dx = Empty
--             | not (inRange (newX, newY) cellsList) || value (nodeIndexer (newX, newY) cellsList)  /= 0 = recursiveCall (index+1)
--             | canDisconnect (cells newBoard) x y && connected(cells newBoard) newX newY < last - start = recursiveCall(index+1)
--             | solution /= Empty = solution
--             | otherwise = recursiveCall (index + 1)
--             where
--                 (newX, newY) = (x + getElement dx index, y + getElement dy index )
--                 solution = fillRecursive newBoard newStart (newX, newY) last

connected :: [Node] -> Int -> Int -> Int
connected board x y
    | value (nodeIndexer (x, y) board) /= 0 = 0
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
        zeros = filter (\x -> value x==0) [nodeIndexer (x + getElement dx i, y+getElement dy i ) board | i <- [0..length dx-1], inRange (x+getElement dx i,y+getElement dy i) board]

        separatesC = (not (inRange (up,y) board) || value (nodeIndexer (up,y) board) /= 0)  &&
                     (not (inRange (down,y) board) || value (nodeIndexer (down,y) board) /= 0) &&
                     ((inRange (up,left) board && value (nodeIndexer (up,left) board) == 0) ||
                      (inRange (down,left) board && value (nodeIndexer (down,left) board) == 0) || value (nodeIndexer (x,left) board) == 0) &&
                     ((inRange (up,right) board && value (nodeIndexer (up,right) board) == 0) ||
                      (inRange (down,right) board && value (nodeIndexer (down,right) board) == 0) || value (nodeIndexer (x,right) board) == 0)
        separatesR = (not (inRange (x,left) board) || value (nodeIndexer (x,left) board) /= 0) &&
                     (not (inRange (x,right) board) || value (nodeIndexer (x,right) board) /= 0) &&
                    ((inRange (up,left) board  && value (nodeIndexer (up,left) board) == 0 ) ||
                     (inRange (up,right) board && value (nodeIndexer (up,right) board) == 0) || value (nodeIndexer (up,y) board) == 0) &&
                    ((inRange (down, left) board && value (nodeIndexer (down,left) board) == 0 ) ||
                     (inRange (down, right) board && value (nodeIndexer (down,right) board) == 0) || value (nodeIndexer (down,y) board) == 0)


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

-- generateBoard :: Int -> Int -> Maybe Board
generateBoard a b = do

    -- randShape <- randomRIO (0,2)
    let shapes = [squareBoard a b]

    --  change head shapes for indexing on shape
    let selected = head shapes

    let blankBoard = selected

    let blankCells = generateCoordinates (cells selected) 1 0
    randIndex <- randomRIO (0,length blankCells -1)
    secondRand <- randomRIO (0,length blankCells -1)

    let newCells = removeNodeByPosition (cells selected) (blankCells !! randIndex)
    let newBoard = Board (Node 1 (blankCells !! randIndex) : newCells) (minNum selected) (maxNum selected)

    let newCells2 = removeNodeByPosition (cells newBoard) (blankCells !! secondRand)
    let max = length newCells2+1
    let newBoard2 = Board (Node max (blankCells !! secondRand) : newCells) (minNum selected) (maxNum selected)

    --- Fill the blank board
    let board = solve newBoard2


    --- get the filled positions list
    let filledList = generateCoordinates(cells board) (maxNum board) 2
    let n = length filledList

    let list = map(`mod` n) (randomList n (mkStdGen 1))
    let deleteList = sort (zip list filledList)
    let targetNum = maxNum board `div` 2
    let generatedBoard = removeNum board deleteList targetNum
    -- printBoard generatedBoard
    return generatedBoard

