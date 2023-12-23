import qualified Data.Map as M
import qualified Data.Set as S
import Data.List (nub,(\\), sortOn, intercalate)
import Data.Maybe (isNothing, fromJust, isJust, mapMaybe)
import Algorithm.Search (dijkstra)

type Pos = (Int, Int)
type Grid = M.Map Pos Char
type Gps = M.Map Pos Int
type Graph = M.Map Pos [(Int,Pos)]

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

neighbours :: Pos -> [Pos]
neighbours (x,y) = [(x-1,y),(x+1,y),(x,y-1),(x,y+1)]

neighboursIn :: Grid -> Pos -> [Pos]
neighboursIn grid pos = filter (isJust . (`M.lookup` grid)) (neighbours pos)

nodes :: Grid -> [Pos]
nodes maze = filter isNode $ M.keys maze
    where isNode n@(x,y) = length (nextDoor n) >= 3 || y == 0 || y == yMax
          nextDoor = mapMaybe (`M.lookup` maze) . neighbours
          yMax = maximum $ map snd $ M.keys maze

nextNodes :: Grid -> [Pos] -> Pos -> [(Int,Pos)]
nextNodes grid goals start = mapMaybe findPath next
    where next = neighboursIn grid start
          findPath = fmap extract . dijkstra ((\\ [start]) . neighboursIn grid) (\a b -> 1) (`elem` goals)
          extract (cost, path) = (1+cost, last path)

showPos :: Pos -> String
showPos (x,y) = "n"++show x++"_"++show y

display :: (Pos,[(Int,Pos)]) -> String
display (p,ps) = showPos p ++ " -> " ++ intercalate "," (map (showPos.snd) ps)

toGraph :: Grid -> Graph
toGraph grid = M.fromList $ zip allNodes $ map (nextNodes grid allNodes) allNodes
    where allNodes = nodes grid

pathsBetween :: Graph -> Pos -> Pos -> [Int]
pathsBetween graph from to = concatMap proceed ways
    where ways = M.findWithDefault [] from graph
          proceed (cost,dest) | dest == to = [cost]
          proceed (cost,dest) = map (cost+) $ pathsBetween (M.delete from graph) dest to

main = do
    maze <- M.filter (/= '#') . asMap . lines <$> readFile "day23.txt"
    let graph = toGraph maze
        yMax = maximum $ map snd $ M.keys maze
    print $ maximum $ pathsBetween graph (1,0) (yMax-1,yMax)
