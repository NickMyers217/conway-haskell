import Data.List

-- | A Game consists of a 2D list of Cells
-- | A GameString is a list of Strings
-- | A Cell is simply a status, either Alive or Dead
type Game = [[Cell]]
type GameString = [String]
type Cell = Status
data Status = Alive | Dead deriving (Eq, Show)


-- | A starter GameString
game :: GameString
game = ["----------"
       ,"---ooo----"
       ,"----------"
       ,"----------"
       ,"----------"
       ,"----------"
       ,"----------"
       ,"---ooo----"
       ,"----------"
       ,"----------"]


-- | Functions to convert a GameString to a Game
readCell :: Char -> Cell
readCell '-' = Dead
readCell 'o' = Alive
readCell  _  = error "Invalid game character... Moron."

readGame :: GameString -> Game
readGame []   = error "Game is empty... Idiot."
readGame game = map readRow game
    where readRow = map readCell


-- | Functions to convert a Game to a GameString then print it
showCell :: Cell -> Char
showCell c
    | c == Dead  = '-'
    | c == Alive = 'o'

showGame :: Game -> GameString
showGame game = map showRow game
    where showRow = map showCell

printGame :: GameString -> IO ()
printGame game = putStrLn $ intercalate "\n" game


-- | Functions that move a Game forward a generation
-- | 1) Any live Cell with fewer than two live neighbours dies, as if caused by under-population.
-- | 2) Any live Cell with two or three live neighbours lives on to the next generation.
-- | 3) Any live Cell with more than three live neighbours dies, as if by overcrowding.
-- | 4) Any dead Cell with exactly three live neighbours becomes a live cell, as if by reproduction.

-- | Vectors to get at neighboring cells
neighbors :: [(Int, Int)]
neighbors  = [(-1,-1), (0,-1), (1,-1), (-1,0), (1,0), (-1,1), (0,1), (1,1)]


-- | Takes a Cell Status and the # of neighbors the cell has and returns the new Cell
nextStatus :: Status -> Int -> Cell
nextStatus s n
    | s == Alive && n < 2              = Dead
    | s == Alive && (n == 2 || n == 3) = Alive
    | s == Alive && n > 3              = Dead
    | s == Dead  && n == 3             = Alive
    | otherwise                        = Dead


-- | Takes a Game, the (x, y) coordinates for a cell and returns the new Cell
checkCell :: Game -> (Int, Int) -> Cell
checkCell g (x, y) = nextStatus status nCount
    where coords = map (\(dx, dy) -> (x + dx, y + dy)) neighbors
          inside = filter (\(nx, ny) -> nx >= 0 && ny >= 0 && nx < length game && ny < length game) coords
          alive  = filter (\(nx, ny) -> g !! ny !! nx == Alive) inside
          status = g !! y !! x
          nCount = length alive


-- | Takes a Game and returns the next gen Game
nextGame :: Game -> Game
nextGame g = map (\lst -> map (checkCell g) lst) (gameCoords 0 [[]])
    where gLen = length game
          gameCoords row acc
              | row == gLen = drop 1 acc
              | otherwise   = gameCoords (row + 1) (acc ++ [[ (x,y) | x <- [0..gLen - 1], y <- [row] ]])



















