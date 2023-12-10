import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe (mapMaybe, isJust, fromJust)

type Pos = (Int,Int)
type Grid = M.Map Pos Char

converge :: Eq a => (a -> a) -> a -> a
converge = until =<< ((==) =<<)

asMap :: [String] -> Grid
asMap lines = M.fromList [((x,y), lines !! y !! x) | y <- [0..length lines-1], x <- [0..length (head lines)-1]]

connects :: Char -> Pos -> [(Pos,[Char])]
connects 'F' (x,y) = [((x+1,y),"7-J") , ((x,y+1),"J|L")]
connects '7' (x,y) = [((x-1,y),"L-F" ), ((x,y+1),"J|L")]
connects 'J' (x,y) = [((x-1,y),"L-F") , ((x,y-1),"7|F")]
connects 'L' (x,y) = [((x+1,y),"7-J") , ((x,y-1),"7|F")]
connects '-' (x,y) = [((x+1,y),"7-J") , ((x-1,y),"L-F")]
connects '|' (x,y) = [((x,y-1),"7|F") , ((x,y+1),"J|L")]
connects _ _ = []

grow :: Grid -> Maybe (S.Set Pos,[Pos]) -> Maybe (S.Set Pos,[Pos])
grow grid Nothing = Nothing
grow grid (Just (loop, edge)) = if checks then Just (S.union loop (S.fromList goodEnds),goodEnds) else Nothing
    where ends = concat $ zipWith connects (map (\pos -> M.findWithDefault '.' pos grid) edge) edge
          goodEnds = S.toList $ S.difference (S.fromList $ map fst ends) loop
          good (pos,allowed) = M.findWithDefault '.' pos grid `elem` allowed
          checks = all good ends

flood :: Grid -> (S.Set Pos, S.Set Pos) -> (S.Set Pos, S.Set Pos)
flood grid (seen, edge) = (S.union seen edge, newEdge)
    where neighbours (x,y) = [(x-1,y),(x+1,y),(x,y-1),(x,y+1)]
          goVisit (x,y) = filter (\pos -> M.findWithDefault 'x' pos grid `elem` "._") $ neighbours (x,y)
          maybeEdge = S.fromList $ concatMap goVisit edge
          newEdge = S.difference maybeEdge seen

findLoop :: Grid -> (Grid, Maybe (S.Set Pos))
findLoop grid = head $ filter (isJust.snd) $ map tryFindLoop candidates
    where start = head $ M.keys $ M.filter ('S'==) grid
          candidates = map (\c -> M.insert start c grid) "F7JL-|"
          tryFindLoop oneGrid = (oneGrid, fmap fst $ converge (grow oneGrid) $ Just (S.empty, [start]))

part1 :: Grid -> Int
part1 grid = length loop `div` 2
    where (_, Just loop) = findLoop grid

part2 :: Grid -> Int
part2 grid = length $ filter ('.'==) $ M.elems inside
    where (goodGrid, Just loop) = findLoop grid
          expanded = asMap $ double $ display $ M.restrictKeys goodGrid loop
          outside = fst $ converge (flood expanded) (S.empty, S.singleton (0,0))
          inside = M.withoutKeys expanded outside

main = do
    grid <- asMap . lines <$> readFile "day10.txt"
    print $ part1 grid
    print $ part2 grid


display :: Grid -> [String]
display scan = [ [ rep $ M.lookup (x,y) scan | x <- [-1..xMax+1]] | y <- [-1..yMax+1]]
  where rep (Just c) = c
        rep Nothing = '.'
        (xMax,yMax) = (maximum $ map fst $ M.keys scan, maximum $ map snd $ M.keys scan)

double :: [String] -> [String]
double = concatMap ((\s -> s : [map mapV s]) . doubleH)
    where doubleH = concatMap (\c -> c : [mapH c])
          mapH 'F' = '-'
          mapH 'L' = '-'
          mapH '-' = '-'
          mapH _ = '_'
          mapV '7' = '|'
          mapV 'F' = '|'
          mapV '|' = '|'
          mapV _ = '_'