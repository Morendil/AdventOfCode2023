import qualified Data.Map as M
import Data.List

type Grid = M.Map (Int, Int) Char

asMap :: [String] -> Grid
asMap lines = M.fromList [((x,y), lines !! y !! x) | y <- [0..length lines-1], x <- [0..length (head lines)-1]]

double :: [String] -> [String]
double = concatMap (\l -> if '#' `elem` l then [l] else [l,l])

expand :: [String] -> [String]
expand = transpose . double . transpose . double

part1 :: Grid -> Int
part1 grid = sum (map dist $ pairs galaxies)
    where dist ((x1,y1),(x2,y2)) = abs (x2-x1) + abs (y2-y1)
          pairs l = [(one,two) | n <- [0..length l-1], let one = l !! n, n2 <- [(n+1)..length l-1], let two = l !! n2]
          galaxies = M.keys $ M.filter (=='#') grid

main = do
    grid <- asMap . expand . lines <$> readFile "day11.txt"
    print $ part1 grid

