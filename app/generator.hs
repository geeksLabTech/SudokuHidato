module Generator where

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

fillRecursive :: Board -> Int -> (Int, Int) -> Int -> Board
fillRecursive board start (x,y) last
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
            | not (inRange (newX, newY) cellsList) || getElement (getElement cellsList newX) newY /= 0 = recursiveCall (index+1)
            | canDisconnect (cells newBoard) x y && connected (cells newBoard) newX newY < last - start = recursiveCall(index+1)
            | solution /= Empty = solution
            | otherwise = recursiveCall (index + 1)
            where
                (newX, newY) = (x + getElement(dx,index), y+ getElement(dy, index))
                solution = fillR newBoard newStart (newX, newY) last

removeNum :: Board -> [(Int, (Int, Int))] -> Int -> Board
removeNum board list n
    | n == 0 = board
    | otherwise = removeNum newBoard rest (n-1)
    where
        (init:rest) = list
        (index,pos) = init
        newCells = replaceAt pos 0 (cells board)
        newBoard = Board newCells (minNum board) (maxNum board)

randomList :: Int -> StdGen -> [Int]
randomList n = tale n . unfoldr (Just . random)

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
