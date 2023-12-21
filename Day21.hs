import qualified Data.Map as M
import qualified Data.Set as S

type Pos = (Int, Int)
type Grid = M.Map Pos Char

asMap :: [String] -> Grid
asMap lines = M.fromList [((x,y), lines !! y !! x) | y <- [0..length lines-1], x <- [0..length (head lines)-1]]

walk :: Grid -> S.Set Pos -> S.Set Pos
walk grid edge = S.fromList $ concatMap goVisit edge
    where neighbours (x,y) = [(x-1,y),(x+1,y),(x,y-1),(x,y+1)]
          goVisit (x,y) = filter (\pos -> M.findWithDefault 'x' pos grid == '.') $ neighbours (x,y)

part1 :: Grid -> Int
part1 garden = 1 + length (last $ take 65 $ iterate (walk garden) (S.singleton start))
    where start = head $ M.keys $ M.filter ('S' ==) garden

main = do
    garden <- asMap . lines <$> readFile "day21.txt"
    print $ part1 garden
