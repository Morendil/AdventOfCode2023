import qualified Data.Map as M
import qualified Data.Set as S
import Data.List (nub)
import Data.List.HT ( takeUntil )

data Dir = North | South | East | West deriving (Eq, Show, Enum, Ord)
type Pos = (Int, Int)
type Grid = M.Map Pos Char
type Beam = (Pos, Dir)

asMap :: [String] -> Grid
asMap lines = M.fromList [((x,y), lines !! y !! x) | y <- [0..length lines-1], x <- [0..length (head lines)-1]]

grow :: Grid -> (S.Set Beam, [Beam]) -> (S.Set Beam, [Beam])
grow grid (trace, edge) = (nextTrace, nextEdge)
    where nextEdge = S.toList $ S.difference (S.fromList $ concatMap (move grid) edge) trace
          nextTrace = S.union trace (S.fromList edge)

move :: Grid -> Beam -> [Beam]
move grid beam@(pos, dir) = filter (onGrid.fst) $ map advance $ transform (tile pos) beam
    where offsets = [(0,-1),(0,1),(1,0),(-1,0)]
          tile p = M.findWithDefault ' ' p grid
          onGrid p = tile p /= ' '
          advance (p,d) = (add p $ offsets !! fromEnum d, d)
          add (x1,y1) (x2,y2) = (x1+x2,y1+y2)

transform :: Char -> Beam -> [Beam]
transform '/' (p,dir) = [(p,toEnum $ (fromEnum dir + 2) `mod` 4)]
transform '\\' (p,dir) = [(p,toEnum $ (3 - fromEnum dir) `mod` 4)]
transform '|' (p,dir) | dir `elem` [East, West] = [(p,North),(p,South)]
transform '-' (p,dir) | dir `elem` [North,South] = [(p,East),(p,West)]
transform ' ' beam = []
transform _ beam = [beam]

part1 :: Grid -> Int
part1 contraption = energize contraption ((0,0),East)

part2 :: Grid -> Int
part2 contraption = maximum $ map (energize contraption) (starts contraption)

starts :: Grid -> [Beam]
starts grid = concat [top,left,bottom,right]
    where (xMax,yMax) = (maximum $ map fst $ M.keys grid, maximum $ map snd $ M.keys grid)
          top = [((x,0),South) | x <- [0..xMax]]
          bottom = [((x,yMax),North) | x <- [0..xMax]]
          left = [((0,y),East) | y <- [0..yMax]]
          right = [((xMax,y),West) | y <- [0..yMax]]

energize :: Grid -> Beam -> Int
energize grid start = length energized
    where final = last $ takeUntil (null.snd) $ iterate (grow grid) (S.empty, [start])
          energized = nub $ map fst $ S.toList $  fst final
main = do
    contraption <- asMap . lines <$> readFile "day16.txt"
    print $ part1 contraption
    print $ part2 contraption