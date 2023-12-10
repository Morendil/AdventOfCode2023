import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe (mapMaybe)

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

findLoop :: Grid -> [Pos]
findLoop grid = S.toList $ fst $ head $ mapMaybe tryFindLoop candidates
    where start = head $ M.keys $ M.filter ('S'==) grid
          candidates = map (\c -> M.insert start c grid) "F7JL-|"
          tryFindLoop oneGrid = converge (grow oneGrid) $ Just (S.empty, [start])

part1 :: Grid -> Int
part1 grid = length (findLoop grid) `div` 2

main = do
    grid <- asMap . lines <$> readFile "day10.txt"
    print $ part1 grid