import qualified Data.Map as M
import qualified Data.Set as S
import Data.Char (intToDigit)
import Data.Maybe (isNothing, catMaybes, fromJust)
import Data.List (nub, partition)
import Data.List.Extra (minimumOn)
import Data.Bifunctor (bimap)

type Pos = (Int, Int)
type Grid = M.Map Pos Char
type Roadmap = M.Map Pos Int

asMap :: [String] -> Grid
asMap lines = M.fromList [((x,y), lines !! y !! x) | y <- [0..length lines-1], x <- [0..length (head lines)-1]]

walk :: Grid -> S.Set Pos -> S.Set Pos
walk grid edge = S.fromList $ concatMap goVisit edge
    where neighbours (x,y) = [(x-1,y),(x+1,y),(x,y-1),(x,y+1)]
          goVisit (x,y) = filter (\pos -> M.findWithDefault 'x' pos grid == '.') $ neighbours (x,y)

part1 :: Grid -> Int
part1 garden = 1 + length (last $ take 65 $ iterate (walk garden) (S.singleton start))
    where start = head $ M.keys $ M.filter ('S' ==) garden

display :: Roadmap -> [String]
display grid = [ [ rep $ M.lookup (x,y) grid | x <- [xMin..xMax]] | y <- [yMin..yMax]]
  where rep (Just i) = intToDigit (i `mod` 16)
        rep Nothing = '#'
        (xMax,yMax) = (maximum $ map fst $ M.keys grid, maximum $ map snd $ M.keys grid)
        (xMin,yMin) = (minimum $ map fst $ M.keys grid, minimum $ map snd $ M.keys grid)

displayG :: Grid -> [String]
displayG grid = [ [ rep $ M.lookup (x,y) grid | x <- [xMin..xMax]] | y <- [yMin..yMax]]
  where rep (Just c) = c
        rep Nothing = '#'
        (xMax,yMax) = (maximum $ map fst $ M.keys grid, maximum $ map snd $ M.keys grid)
        (xMin,yMin) = (minimum $ map fst $ M.keys grid, minimum $ map snd $ M.keys grid)

color :: Pos -> Grid -> Roadmap
color pos grid = go 0 M.empty [pos]
    where neighbours (x,y) = [(x-1,y),(x+1,y),(x,y-1),(x,y+1)]
          free g p = M.findWithDefault 'x' p grid == '.' && isNothing (M.lookup p g)
          paint n = foldl (\g p -> M.insert p n g)
          go n g [] = g
          go n g ps = go (n+1) (paint n g ps) next
            where next = nub $ concatMap (filter (free g) . neighbours) ps

bounds :: M.Map Pos a -> (Int, Int)
bounds g = (maximum $ map fst $ M.keys g, maximum $ map snd $ M.keys g)

mins :: M.Map Pos a -> (Int, Int)
mins g = (minimum $ map fst $ M.keys g, minimum $ map snd $ M.keys g)

triple :: Grid -> Grid
triple g = M.fromList $ [((x,y),fromJust $ M.lookup (x`mod`(xMax+1),y`mod`(yMax+1)) g) | x <- [(-xMax-1)*8..(xMax+1)*9-1], y <- [(-yMax-1)*8..(yMax+1)*9-1]]
    where (xMax,yMax) = (maximum $ map fst $ M.keys g, maximum $ map snd $ M.keys g)

border :: Roadmap -> Roadmap
border grid = M.filterWithKey (\(x,y) a -> x == xMin || y == yMin || x == xMax || y == yMax) grid
    where (xMax,yMax) = bounds grid
          (xMin,yMin) = mins grid

rightBorder :: Roadmap -> Roadmap
rightBorder grid = M.filterWithKey (\(x,y) a -> x == xMax) grid
    where (xMax,yMax) = bounds grid
          (xMin,yMin) = mins grid

main = do
    garden <- asMap . lines <$> readFile "day21.txt"
    let start = head $ M.keys $ M.filter ('S' ==) garden
        blank = M.insert start '.' garden
        gardens = last $ take (262) $ iterate (walk blank) (S.singleton start)
        gardenMap = M.fromSet (const 'O') gardens
        final = M.union gardenMap blank
    putStrLn $ unlines $ displayG final
    print $ length gardens
    --     rocks = M.filter ('#'==) garden
    -- print $ bimap length length $ partition even $ map (uncurry (+)) $ M.keys rocks
