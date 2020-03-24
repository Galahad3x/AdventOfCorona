start_x = 12
start_y = 42

data MapPoint = MapPoint { x_cord :: Int, y_cord :: Int, pointType :: Char, searchStatus :: Bool } deriving (Show, Eq)

type Maze = [MapPoint]

data MappTree = EmptyTree | MappTree { nodeVal :: MapPoint, height :: Int, leaves :: [MappTree] }  deriving (Show,Eq)

main = do
    let n = 100
    let start_x = 12
    let start_y = 42
    let maze1 = buildMaze theMaze
    let maze = addNs maze1
    putStrLn $ show $ maze
    let bfs_res = bfs_search maze (start_x,start_y)
    putStrLn $ show $ bfs_res

bfs_search :: Maze -> (Int,Int) -> Int
bfs_search maze point = searchInTree $ bfs maze 0 point

searchInTree :: MappTree -> Int
searchInTree tree = if pointType (nodeVal tree) == 'C' then height tree else minimum $ map (searchInTree) (leaves tree) 

bfs :: Maze -> Int -> (Int,Int) -> MappTree
bfs maze lvl (xc,yc) = if listOfGoodNbs == [] then EmptyTree else MappTree { nodeVal = thePoint, height = lvl, leaves = (map (bfs maze (lvl+1)) listOfGoodNbs) }
    where listOfGoodNbs = filter (goodPoint maze ) [(xc+1,yc),(xc-1,yc),(xc,yc+1),(xc,yc-1)]
          thePoint = (givePoint (xc,yc) maze)

goodPoint :: Maze -> (Int,Int) -> Bool
goodPoint maze addr = (pointType (givePoint addr maze)) `elem` ['.','C']

beenInBFS :: Maze -> (Int,Int) -> Maze
beenInBFS maze addr@(xc,yc) = MapPoint { x_cord = xc, y_cord = yc, pointType = (pointType (givePoint addr maze)) , searchStatus = True }:(newMaze' addr maze)

isCord :: Int -> (MapPoint -> Int) -> MapPoint -> Bool
isCord c f p = c == (f p)

newMaze' :: (Int,Int) -> Maze -> Maze
newMaze' addr@(xc,yc) (x:xs) = if ((x_cord x) == xc) && ((y_cord x) == yc) then xs else x:(newMaze' addr xs) 

givePoint :: (Int,Int) -> Maze -> MapPoint
givePoint (xc,yc) maze = head $ filter (isCord xc x_cord) $ filter (isCord yc y_cord ) maze

addNs :: Maze -> Maze
addNs [] = []
addNs maz@(x:xs)
    | MapPoint { x_cord = ((x_cord x)+1), y_cord = (y_cord x), pointType = 'I', searchStatus = False } `elem` maz = MapPoint { x_cord = (x_cord x), y_cord = (y_cord x), pointType = 'N', searchStatus = False }:(addNs xs)
    | MapPoint { x_cord = ((x_cord x)-1), y_cord = (y_cord x), pointType = 'I', searchStatus = False } `elem` maz = MapPoint { x_cord = (x_cord x), y_cord = (y_cord x), pointType = 'N', searchStatus = False }:(addNs xs)
    | MapPoint { x_cord = (x_cord x), y_cord = ((y_cord x)+1), pointType = 'I', searchStatus = False } `elem` maz = MapPoint { x_cord = (x_cord x), y_cord = (y_cord x), pointType = 'N', searchStatus = False }:(addNs xs)
    | MapPoint { x_cord = (x_cord x), y_cord = ((y_cord x)+1), pointType = 'I', searchStatus = False } `elem` maz = MapPoint { x_cord = (x_cord x), y_cord = (y_cord x), pointType = 'N', searchStatus = False }:(addNs xs)
    | otherwise = x:(addNs xs)
    
buildMaze :: [[Char]] -> Maze
buildMaze mazetext = buildMaze' mazetext 0

buildMaze' :: [[Char]] -> Int -> [MapPoint]
buildMaze' [] _ = []
buildMaze' (x:xs) x_cont = (getMapPoints x x_cont 0 ) ++ buildMaze' xs (x_cont+1)

getMapPoints :: [Char] -> Int -> Int -> [MapPoint]
getMapPoints [] _ _ = []
getMapPoints (x:xs) x_cor y_cor = (createPoint x_cor y_cor x):(getMapPoints xs x_cor (y_cor+1))

createPoint :: Int -> Int -> Char -> MapPoint
createPoint xc yc tp = MapPoint { x_cord = xc, y_cord = yc, pointType = tp, searchStatus = False}

theMaze :: [[Char]]
theMaze = [".....XX....X......XXX.XX.XX...XXXX.......X.X.XX.....XX..X...X.XX.XX.X..XXX.....XX.......X.........XX","..X...........XX...XX.X.....X...X.XX.XX..XX.X..X.X..XXX...XX....X...XX..XX.X.XX....XX...XX.X.XX..X..","..XX.XXX..XX.X...X..X......XX..XX.............X..X..X.XX......XXXX...X....X.XXX.X....C....X..X..XX.X","..XXX.X...X..X...X.X.XX.X.X.XX.X...X..XX.X.XX.X.....XX..X.XX..X...XX...XX....X...X.I....XX..X.......","..XXX..X.XXX....X.......X..X......X.X.X....X.XX..X...XXX..XX..X..XX...XXX.X.........XX.X....X..X.X..",".....XX...X...X..X.XX..XXXXX....X......XXX..X.X..X.XXX.X...XXX...X.X.X.X..X..X.X.X...X.XXX..X..XXX.X","......XX...X.XXXX..X..X..XXX.....X.XX.X....X.X..X....XXX.....X...X..XX..X..XXXX................X...X","...XX...XX...X.XX........X..X....X......XX..X.XX...XX..X..X.X.....XX...X...XX..X.X.XXXX.XXXX.XXX..XX",".X.......X.X.....X.X..X..X..X...X.X.X..XXXX.X..X.....X...........XX.....X...X..XX.XX..X.....X.X.X...",".......X..I..XX...XXX.X.XX....XX.X.........XX.X.X...XX..X...X..X.X..XX.XX.X.......X...X.XX..XX......","XXX.....X.......XXX..X.X..X.X.X.XX....X.X..X....XX..X....I...XXXX..X.X..X.X..XX...X....X..X.X.XX.XX.",".....I....X...X...XX...X.X.XXX.X.XX...XX............X.X.XXXXXX..X.X.X..X..X..XX.......X.X.X.X...X...","......X......X.XX...X...X..X..X....X...XX...X.X.X...........X.....X.XXXXX..X.X..X....X.X.X...XX.....","..X.X..X....XXXX.XX...X.....X.XXXX...X..XX..X.X.........X..X.XXX.X.XX.XXX...X....X..X.X..X..X....X..","XXX.X..XX.X.......XX.XXX..XX.XXX..XX.X...X..XX.....X...........XX.X...X.X..X.....XXX.....X.........X","XXX....X.X.XXX.X....X..XX........XX.X...X.X....X...XX........X.X...XX.X.X..XX.....X.X.X....X..X.X.X.","......X..X.......I........X.X.X..XX..X.X.X...X...X..X...X.X.X....XX....XX......XX.X....X.I...X.XXXX.",".X....X......X.XXXXXXX.X.XX..XXX.XX..X..XXXX...X....X.XX..X...X.XX.X.XX..X..X.XX.XX..I....X.XX.X.X.X","..XXXX......X...X.........X.XX..XX..X..X........X.X.XX.X..X..XXX.X.X...X......X...X.....X.XX...XX.X.","X..XX...X....XX...XX.X.XX.........XX.....XX............XX.....X.XX.....X.X.X..X..X..XX.XXXX.XXX..X..","X....X...X.X.X..X........XX...X....XX.X....X..XXXX...XX....I....X..X.XX.XX...X...X.X.XX.....XX..XX.X","..XX....X...X..X.XXX.X.X.X.....X.....X......XX..X.X..XXX....X.....X...........X...X....X.....X.X.X..","..XX.X..X.X...X.X.X.......X..XX.XXXX.XXX.X....X.XX..X.X....XX....X.XXXXX.XXX.XX....X..X..X..XX...XX.","..X.....X.X.XX.X....X......X.XX......X.XX..X....X......XXXX..X..XX.....X....X.X......X..XX.XX...XXX.","..X...X..C..X.......XX...X.....X.XXX...XX.......X.XXXXX.....XX...XX..X..X......XXXXX....XX..XXX...X.","...XXX......X..XX......X.....XXXX.X...X..XX....X.X.X..XX.X.X.X....X..I...X..X..X....XXXXX..XX...X.XX","...X...X.X..XX..XX.XX.X....X.X..X.....XX....XX...X....XXX.XX.X.......X....XX..X.XX...X.XX.X..C...X.X",".....X.X.X.......XXX....XX.......X....X.X....XX.X...X......XX.X...X.....X...X.X......X.X...X.X..X.XX","X.X...X..XX.......X..X.X.XX..X....X...X....X..XX..X.X.XXX...XXX.X.X.XX.....X...X...XX...X.X.X..XX..X","XX.X.X...X.......XXXXX...XX.XX...XX......X.X..X..XXX.XX..XXXX......X.XXX.XX..X..XXXX.X.X.XXXX..X...X",".X.....X...XX....X..X.X.X..XX..X..C...XXX.X..X.XXXX..X...X..X.....X........X.X....X..X.X.X..X.X...X.","X..X...X......X..X.....XX..XXXX...X.XX..XXX..X.X.XX..XX.X.X...XX....X...X.......XX..X.X...X...XX....","..X........X.......X..XX....X.X.X...X.X.....XXXXXXX..X.X.X.X.....XXXX.XX..X.XX...XX.XX.....X.X.X..X.","X...XX.X...X.XX.X...X.X.....XX.......X.XX.............X...X..X.X.X....XX.....X...XX..XX..XX.X..X..X.",".X.XX..XX.X...X...X...XX..........X.......X.X.....X...X.X.X.XX.X..X.XXX.X.X...X......X.......X..X...",".XXX.XXX....X......X...X.I.......X...X....X.XX...XX..X....XX..X.XX...XX..XX.XXX.....X..X..X....X.XX.",".X.XXX.X...XX.X.......X...X.XXX..X.X..X..X....X.....X.X....XX.X.X.X.X..X..XXXXX.X.X.XX.X.....XX.X...",".....X..........XX.XX...X..X..X.XX...X......XX....X.....X.....XX....X.....X.X.X.....X.XX.XXXXXX.....","XX.X..XXX.....X.X.X.X.X.X.....X.........XXX..X....X..XXX.....X.XX.X..X.XXXXX....X.X...............X.","..X..XX....X.X.......X..X.........XX..X.X...XX.XXX..XX.X.X..XXX....X.........X.X.X....X..X.....XX...","..X.X.XX......X..X...X.X......X..X.X.....X..X.X..X.X...XX...XX...X..XXX.X.XXXX........X..XXX.....X.X","................XX......X.X....X.X....X...X......X....X...X..XX....X.XX.XX.X.X.XXXX..X....X.X...I...","XXXX..X.X..X.XX.X..XXXXXX..........X...X..XX..XXX.X.......I................X.X..X..X...X.X.X........","...X.......X.XXXXX...XXX.X.X...X.X....XX.....XXXXX...I...........XX.XX.X.XX.X..X.X.X...X.XX....X.X..",".XXX.....XXX....X...XX.X..XX.......X..X.X.X..XXX..XXX....X...XX.X.........X.X...XX.XX.X..X...X..X...","XX......X.....X.....XX.XXX.XX...XX.X.X.XX..X.......XX....X.X.X.X.......XXXXX.....X.X...X.X.XX..X...X","XXXXX.X.XXX..X...X...X.....X.......X.X..XX.X...XX....X..XX....XX.XXX..XX.X..X........X.XXXX...X...XX","...XX.X.X.X.XXX...XXX.XXX...X.X....I......X.........X.X..XXXX..I....XX..XX....XX...XXX..XX..X...X..X","..XX.X......X....X.X..XX.....XX..X........X.XX.XXXXXX.XX.XXX........X..X...X.XX..XXXX.....XXXX....XX","...X.XXX..XX..X........X.XXXX.......XXXX..X.......X.X....X.....XXX...X.XX.X...X..XX..X.X..X..X....XX","X...X..XXX...X.X.....XXX.....XX.X....X.....XXXXX...XX..X....X.XXX.X...XX..........XX.X..............",".X.XXXX.XX.....XX.XX..X.XXXX.I...X..X..X.....X...X.......X.X.......X.XX..XX..XX........X.XXX...I....","XX........XXX...XXX.X...X...XX....X...X...X...X.XX...XX.XXX.....X..X..XXX.X..X..X....X....X......X..","....XXX...X....XXX...X.XXX..X....X.X.X.X.....XX.X...XX..X....X.XXXX........XX...XXX.X.X..XXX..X.....","X....X.....XX....XX.X.X.X..XX...X.X.X.......X..X.X....XX.X.....X..X.........XXXXX..XX..X.........X.X",".X.X..XX.X.......X..XX..X...XX.X.XX..XX.X...XX.X..X..........X..XXX.XX....X...........X..X..I.......","X....XXXX...X..X.X....X.X..XX.X...XX.......XXXX..XX.....X........XX..X..X....X.X.X.X..XXX........X..","...X...X.XX.X...XX.XX......X.X......X...XXX.X..XX...X......X.....X.XX..X..X..X.X......XX.X.....X....","...X.XXXX..X.......X..X..XX.XXX.X...XXXX........X..XXX....X..X..X.....X..X...X.X......XX....X.X..XX.","....X.....XXX....X....X...X.......X..X..X....X...X......XX.XX......X.......XX.XXX.X.......XX..X.X..X",".XXX......X.XX......X..X....XXX....X..XX....XXXX......X...XXX..X.....XXX.X........X..XX..X.X.X......","X..X..X.X.....X..X.XX....X.XX...........X......XXX...X........X........X...X...X...X..XX....X.......",".X.X.X..XX.XX...X......X.X..XX..X..XXX...XXX....X.XXX...XX.X..........X.X.X....XXX.X.X....X....C....",".....X.XX.X....X.X.X...X.XX.XXX...X.X.....XX.X...XX.XXX...XXXX.XXX.X...XXXX.....XXX..X...I...XX.....","X..XX.........XXX.XX.XX.X.X.X..XX....XX.XX....X...X...X.....X.X.X..X.X.X........X.XXX....X...XXX.X.X",".XX....XXX.X.X....C....X.XX..X....X..X.X.X.XX.X.XX...X.....X........X.....X.X.XX.X...X...XX..X...X.X","X.....X.........X....XX...XX..XX..XX...XX.........XXX.....X.X..X....X.XXX.XX...I.....X.X.X......X..X",".X..X..X....X.X.X.....XX....X.XX..XXXX.XX..I.....X..XX...XX.XXX.I...X.....X.......X......X....X.X..X",".XX.X.X.X.X......XXX.....X....X.....XX.X...X...XX..X....XX......X..C..X.X.X..XX....X..XXX..C.......X",".XX...XX.....X.X......XX.X.X..X.I...X.....XX.X...X...X...XX...XX........XX...X.X..X..X.XXX...X.X.XXX","XX............X..X.X.X..X.X..X..X..XXX....XXX..X..XX...X...X.......X..X.......X.X.X.X..XXXX.X.X.X..X","..X...X.X..X..XXX...X.X..XXX.X.......X.....XX.XX.XX..XXXX.X..XXXX.X.X..XX....XX..X....XX.XXX..X.....","...X....XX..X..X...X..XX.XX...XX..XX......XX....X..XX.X..XX.X..XXX...XX.XX.XX...XX.X.....X...X..XXXX","..X.......XX...X..XX.X..X.......XX...X..X..XX.X....XX....X...X..X....X.X.X...X...X...XXX..X...I..X..","..X..X..........X...X..X..X...X.X..X...XX...X.........X.X.X.X......X...X....X.XX...X.X...X.X....X.X.","...X.......X..X................XX..X.X..X.X.XX....XX.XX....X..X.XX.....X...X..X..XXXX.X.X.X.X.....X.","XX..X...X.....X.X...XXXX.....X.XXX.....X..X......XXX.XX.XXXX.XX...XX.....X..X....XX...X.X..X..XX..XX",".X....X.....X...X.X.......X...XX...X........XXXX..X...X....X...XX...XXX...XXX.X......X....X...XX..XX","....X.X.X.....X...X....XX...XXX........XX..........XX...XXX.X.......X...X.X....C....X..X...XX.X..XX.","...X.X.X...X.XX.....XX...XX.X.....X.XX...X.........X..XX..X.X..I...X...X...X..........X........X..X.","......X....I......XX.X.....XX...X...XX..X..X.I....X......X...XXX.....X.X..XX..I...XXX.......X..X.X.X",".X..X.....X.....X.XXXX....X....X.X..XX.....X....X......X.X........X.....XX..XX.....X..XX.........XXX",".X...X.XX...XXXX..XX..I...C....X....X.X.XX........X......X.X.XX..X.X...X...X.X.XX......XX..X..X.XX..",".........X..XXX.X.....X........XX.XXX....XXX....X.X.X.X...IX...XXXXXXX......X.XX...X..XX....X.X..XXX",".X..X..XX....X.X....X.X..X......X.X....I...X........C....X........X.X..X.XXX....X...XX..X.X.........","X..XX...XX.XX.......XX....XX.X.......X....X...X..............X..X.....X....XX..X..X....X...X.X..X.X.","...XX..X.X..X.X.XX...X....XXX.X.X..XX.XX...X.X..XX..X....X.X.X....X..X.X..XXX.XX.X......XXXXX..X..X.",".X......X.....X.XXX.X..X.X.X......XX.....X.......XXX...XX.X......XXX..X.X.X.XXX....X....XX..X.X.X..X","...X....X..XX..X..X.XX.X.XX.X..X.......X.XX.X.....X...XXXXXX.I...XXX...X.....X...X..XXX.....X...X...",".....X..X..X.X..XX..XXX..X...X...X.X...X....XX...X.X...X.....X..XX...X.XXX...X..XXX.XXX.X....X...X.X","XX.X..X..X...X......X..X.X.XXXX.X...X.X.X..X.X.X.XX..X..X..X.X.X...X...X.....XX.X.X.X..XX.XX.X....XX","XXX........X.....X.X..X..X....X..XX..XXXX..XX..X.......I........XX.X.X..XXX.....X.XXX....X......X...","..X.XXX.X..X......XX.XXX.XX..XX..X...XX.........XXXXXX...X..XXXXX..XX..X.X........X.XX....I..X...X..",".X.X..X..X......X.X..X..X..XXX...X....XX.....XXXX..X....X...XXX..X.X.XXXX..I........I.....X...X.....","XX.XX...X....XX......X.X.X.X....XX.X.XXX........X.XX..X..............XX....XX..X...X.X.X.X..X.X..X.X","..X.X........X...X.X..X..X..XXX.X...X....X..X.X.........X.X....XX.XX..............X.XX...X..X.X..X..","XX...X.X.X.XXX.X...X.....X..XX...X..XX.XX.X......X.XXX.......XX..X....X.....XXX.XXX..XX..X..XX...XX.","....X..XX....X...XXX.....X.X....XX.......XX.X....X.X.X..X..X.X..X.X..X.....X...X.XX..........X...X..","..XX..XX.......X...X.XX..XXX.......XX.XX.XX.............X.X.XX....X..XXXXXX.XX.X..X....XX.X.X...X.XX","XXX.X....X.X...XX.......XX.XX...XX...X......X.X....XX..XX...X.......XXX....X...XX..X...XXX..X......X"]
