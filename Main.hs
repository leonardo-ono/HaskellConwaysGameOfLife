-- Haskell Conway's Game of Life
-- 
-- Written by Leonardo Ono (ono.leo80@gmail.com)
-- May 20, 2020
--
-- References:
-- https://en.wikipedia.org/wiki/Conway%27s_Game_of_Life
-- https://bitstorm.org/gameoflife/
--
-- GHC version 8.6.5
--
-- Dependencies:
-- gloss-1.13.1.1

import Graphics.Gloss
import Data.List
import Data.Maybe

--- Model / Data ---

-- https://github.com/Peter-Slump/game-of-life/blob/master/patterns/gosper-glider-gun.txt
-- https://kseo.github.io/posts/2014-02-06-multi-line-strings-in-haskell.html
cols = 50
rows = 30
gridData = "\
    \..................................................\
    \..................................................\
    \..................................................\
    \...............................X..................\
    \.............................X.X..................\
    \...................XX......XX............XX.......\
    \..................X...X....XX............XX.......\
    \.......XX........X.....X...XX.....................\
    \.......XX........X...X.XX....X.X..................\
    \.................X.....X.......X..................\
    \..................X...X...........................\
    \...................XX.............................\
    \..................................................\
    \..................................................\
    \..................................................\
    \..................................................\
    \..................................................\
    \..................................................\
    \..................................................\
    \..................................................\
    \..................................................\
    \..................................................\
    \..................................................\
    \..................................................\
    \..................................................\
    \..................................................\
    \..................................................\
    \..................................................\
    \..................................................\
    \.................................................."

initialGrid :: [((Int, Int), Int)]
initialGrid = zip [(x, y) | y <- [0..(rows - 1)], x <- [0..(cols - 1)]] (map toInt gridData)
    where toInt '.' = 0
          toInt 'X' = 1

simulateGameOfLife grid = map evolve grid
    where evolve ((x, y), cellState) = ((x, y), newCellState)
              where newCellState = if (cellState, evalNeighbors) `elem` alive then 1 else 0
                    alive = [(0, 3), (1, 2), (1, 3)] 
                    evalNeighbors = sum $ map neighborState neighbors
                    neighborState (x', y') = fromMaybe 0 $ lookup (x' + x, y' + y) grid
                    neighbors = [(x, y) | x <- [-1, 0, 1], y <- [-1, 0, 1], x /= 0 || y /= 0]

update _ _ grid = simulateGameOfLife grid

--- View ---

render grid = scale 1 (-1) $ pictures $ map (toPicture . rescale) grid
    where rescale ((x, y), s) = ((fromIntegral ((x - (cols `div` 2)) * 10)
                                , fromIntegral ((y - (rows `div` 2)) * 10)), s)
          toPicture ((x, y), s) = translate x y $ case s of 0 -> rectangleWire  10 10
                                                            1 -> rectangleSolid 10 10

main = simulate window background fps initialGrid render update
    where window = InWindow "Haskell Conway's Game of Life" (800, 600) (50, 50)
          background = white  
          fps = 4
