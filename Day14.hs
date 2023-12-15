import qualified Data.Map as M
import Data.Bifunctor (first)

type Grid = M.Map (Int, Int) Char

asMap :: [String] -> Grid
asMap lines = M.fromList [((x,y), lines !! y !! x) | y <- [0..length lines-1], x <- [0..length (head lines)-1]]

slideNorth :: Grid -> Grid
slideNorth grid = M.union (M.fromList $ map (first slide) $ M.assocs rocks) rest
    where (rocks, rest) = M.partition (=='O') grid
          north (x,y) = map (\v -> M.findWithDefault '.' (x,v) grid) [y-1,y-2..0]
          spaces = length . filter (=='.') . takeWhile (/='#')
          slide (x,y) = (x,y-spaces (north (x,y)))

slideSouth :: Grid -> Grid
slideSouth grid = M.union (M.fromList $ map (first slide) $ M.assocs rocks) rest
    where (rocks, rest) = M.partition (=='O') grid
          south (x,y) = map (\v -> M.findWithDefault '.' (x,v) grid) [y+1..yMax]
          spaces = length . filter (=='.') . takeWhile (/='#')
          slide (x,y) = (x,y+spaces (south (x,y)))
          (xMax,yMax) = (maximum $ map fst $ M.keys grid, maximum $ map snd $ M.keys grid)

slideWest :: Grid -> Grid
slideWest grid = M.union (M.fromList $ map (first slide) $ M.assocs rocks) rest
    where (rocks, rest) = M.partition (=='O') grid
          west (x,y) = map (\h -> M.findWithDefault '.' (h,y) grid) [x-1,x-2..0]
          spaces = length . filter (=='.') . takeWhile (/='#')
          slide (x,y) = (x-spaces (west (x,y)),y)

slideEast :: Grid -> Grid
slideEast grid = M.union (M.fromList $ map (first slide) $ M.assocs rocks) rest
    where (rocks, rest) = M.partition (=='O') grid
          east (x,y) = map (\h -> M.findWithDefault '.' (h,y) grid) [x+1..xMax]
          spaces = length . filter (=='.') . takeWhile (/='#')
          slide (x,y) = (x+spaces (east (x,y)),y)
          (xMax,yMax) = (maximum $ map fst $ M.keys grid, maximum $ map snd $ M.keys grid)

doCycle :: Grid -> Grid
doCycle = slideEast . slideSouth . slideWest . slideNorth

load :: Int -> Grid -> Int
load height = sum . map value . M.keys . M.filter (=='O')
    where value (x,y) = height - y

findCycle :: Eq a => [a] -> ([a],[a])
findCycle xxs = fCycle xxs xxs
  where fCycle (x:xs) (_:y:ys)
         | x == y              = fStart xxs xs
         | otherwise           = fCycle xs ys
        fCycle _      _        = (xxs,[]) -- not cyclic
        fStart (x:xs) (y:ys)
         | x == y              = ([], x:fLength x xs)
         | otherwise           = let (as,bs) = fStart xs ys in (x:as,bs)
        fLength x (y:ys)
         | x == y              = []
         | otherwise           = y:fLength x ys

part1 :: Grid -> Int
part1 grid = load height $ slideNorth grid
    where height = 1 + maximum (map snd $ M.keys grid)

part2 :: Grid -> Int
part2 grid = load height endState
    where height = 1 + maximum (map snd $ M.keys grid)
          (init, loop) = findCycle $ iterate doCycle grid
          target = length init + (1000000000-length init) `mod` length loop
          endState = iterate doCycle grid !! target

main = do
    platform <- asMap . lines <$> readFile "day14.txt"
    print $ part1 platform
    print $ part2 platform

display :: Grid -> [String]
display grid = [ [ rep $ M.lookup (x,y) grid | x <- [0..xMax]] | y <- [0..yMax]]
  where rep (Just c) = c
        rep Nothing = '.'
        (xMax,yMax) = (maximum $ map fst $ M.keys grid, maximum $ map snd $ M.keys grid)
