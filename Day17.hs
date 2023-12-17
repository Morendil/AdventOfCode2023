import qualified Data.Map as M
import Data.Char (digitToInt, intToDigit)
import Algorithm.Search (aStar)
import Data.Maybe (fromJust)

data Dir = North | East | South | West deriving (Eq, Show, Enum, Ord)
type Pos = (Int, Int)
type State = (Pos,(Dir,Int))
type Grid = M.Map Pos Int

asMap :: [String] -> Grid
asMap lines = M.fromList [((x,y), digitToInt $ lines !! y !! x) | y <- [0..length lines-1], x <- [0..length (head lines)-1]]

neighbors :: Pos -> State -> [State]
neighbors (xMax,yMax) ((x,y),(dir,straight)) = filter (inBounds.fst) moves
  where add (x1,y1) (x2,y2) = (x1+x2,y1+y2)
        offsets = [(0,-1),(-1,0),(0,1),(1,0)]
        moves = [(add (x,y) (offsets !! fromEnum d),(d, if d == dir then straight+1 else 0)) | d <- turns, d /= dir || straight < 2]
        turns = [toEnum ((fromEnum dir +1) `mod` 4), toEnum ((fromEnum dir +3) `mod` 4), dir]
        inBounds (x,y) = x >= 0 && y >= 0 && x <= xMax && y <= yMax

toMap :: [State] -> M.Map Pos Char
toMap = M.fromList . map toAssoc
    where chars = "^<v>"
          toAssoc (pos,(dir,_)) = (pos, chars !! fromEnum dir)

display :: M.Map Pos Char -> [String]
display grid = [ [ rep $ M.lookup (x,y) grid | x <- [0..xMax]] | y <- [0..yMax]]
  where rep (Just c) = c
        rep Nothing = '.'
        (xMax,yMax) = (maximum $ map fst $ M.keys grid, maximum $ map snd $ M.keys grid)

debug :: [State] -> Grid -> M.Map Pos Char
debug path grid = M.union (toMap path) (M.map intToDigit grid)

part1 :: Grid -> Int
part1 city = fst path
  -- Start at -1 straight line distance because the starting point doesn't count…
  where start = ((0,0),(South,-1))
        goal = (maximum $ map fst $ M.keys city, maximum $ map snd $ M.keys city)
        bounds = goal
        dist (x1,y1) (x2,y2) = abs (x2-x1) + abs (y2-y1)
        path = fromJust $ aStar (neighbors bounds) (const (fromJust . (`M.lookup` city) . fst)) (dist goal . fst) ((==) goal . fst) start

main = do
    city <- asMap . lines <$> readFile "day17_sample.txt"
    print $ part1 city
