import qualified Data.Map as M
import Data.Bifunctor (first)

type Grid = M.Map (Int, Int) Char

asMap :: [String] -> Grid
asMap lines = M.fromList [((x,y), lines !! y !! x) | y <- [0..length lines-1], x <- [0..length (head lines)-1]]

slideNorth :: Grid -> Grid
slideNorth grid = M.fromList $ map (first slide) $ M.assocs rocks
    where rocks = M.filter (=='O') grid
          above (x,y) = map (\h -> M.findWithDefault '.' (x,h) grid) [y-1,y-2..0]
          spaces = length . filter (=='.') . takeWhile (/='#')
          slide (x,y) = (x,y-spaces (above (x,y)))

load :: Int -> Grid -> Int
load height = sum . map value . M.keys
    where value (x,y) = height - y

part1 :: Grid -> Int
part1 grid = load height $ slideNorth grid
    where height = 1 + maximum (map snd $ M.keys grid)

main = do
    platform <- asMap . lines <$> readFile "day14.txt"
    print $ part1 platform
    
display :: Grid -> [String]
display scan = [ [ rep $ M.lookup (x,y) scan | x <- [0..xMax]] | y <- [0..yMax]]
  where rep (Just c) = c
        rep Nothing = '.'
        (xMax,yMax) = (maximum $ map fst $ M.keys scan, maximum $ map snd $ M.keys scan)
