import qualified Data.Map as M
import qualified Data.Set as S
import Data.List (nub,(\\))
import Data.Maybe (isNothing, fromJust)

type Pos = (Int, Int)
type Grid = M.Map Pos Char
type Gps = M.Map Pos Int

asMap :: [String] -> Grid
asMap lines = M.fromList [((x,y), lines !! y !! x) | y <- [0..length lines-1], x <- [0..length (head lines)-1]]

color :: Pos -> Grid -> Gps
color pos grid = go 0 M.empty [] [pos]
    where neighbours (x,y) = case get (x,y) of 
            '.' -> [(x-1,y),(x+1,y),(x,y-1),(x,y+1)]
            '>' -> [(x+1,y)]
            'v' -> [(x,y+1)]
          free n g p = get p `elem` ".>v" && n > M.findWithDefault (-1) p g
          get p = M.findWithDefault 'x' p grid
          paint n = foldl (\g p -> M.insert p n g)
          go n g from [] = g
          go n g from ps = go (n+1) (paint n g ps) ps next
            where next = nub $ concatMap (filter (free n g) . (\\ from) . neighbours) ps

part1 :: Grid -> Int
part1 maze = fromJust $ M.lookup (xMax-1,yMax) colored
    where (xMax,yMax) = (maximum $ map fst $ M.keys maze, maximum $ map snd $ M.keys maze)
          colored = color (1,0) maze

main = do
    maze <- asMap . lines <$> readFile "day23.txt"
    print $ part1 maze
