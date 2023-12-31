import Text.ParserCombinators.ReadP
import Data.Char (isNumber, isAlpha, isHexDigit, digitToInt)
import Data.Maybe (fromJust)
import Numeric (readHex)
import Data.List (sortOn, sort, group, elemIndex, nub)
import Data.List.Extra (groupOn, chunksOf)
import Data.List.HT (rotate)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Bifunctor (second)

type Pos = (Int,Int)
type Instruction = ((Char,Int),String)
type Lagoon = M.Map Pos String
type Grid = M.Map Pos Char

type BigDig = (Int, Int)

instructions :: ReadP [Instruction]
instructions = sepBy1 instruction (string "\n")
instruction :: ReadP Instruction
instruction = do
    c <- get
    string " "
    n <- read <$> many1 (satisfy isNumber)
    string " "
    s <- between (string "(#") (string ")") (many1 (satisfy isHexDigit))
    return ((c,n),s)

dig :: (Pos,Lagoon) -> Instruction -> (Pos,Lagoon)
dig (pos,state) ((dir,len),color) = (last steps, M.union state $ M.fromList [(p,color) | p <- steps])
  where offset 'R' = (1,0)
        offset 'L' = (-1,0)
        offset 'U' = (0,-1)
        offset 'D' = (0,1)
        offset _ = error "offset"
        add (x1,y1) (x2,y2) = (x1+x2,y1+y2)
        steps = take (len+1) $ iterate (add (offset dir)) pos

flood :: Grid -> (S.Set Pos, S.Set Pos) -> (S.Set Pos, S.Set Pos)
flood grid (seen, edge) = (S.union seen edge, newEdge)
    where neighbours (x,y) = [(x-1,y),(x+1,y),(x,y-1),(x,y+1)]
          goVisit (x,y) = filter (\pos -> M.findWithDefault 'x' pos grid == '.') $ neighbours (x,y)
          maybeEdge = S.fromList $ concatMap goVisit edge
          newEdge = S.difference maybeEdge seen

converge :: Eq a => (a -> a) -> a -> a
converge = until =<< ((==) =<<)

asMap :: [String] -> Grid
asMap lines = M.fromList [((x,y), lines !! y !! x) | y <- [0..length lines-1], x <- [0..length (head lines)-1]]

part1 :: [Instruction] -> Int
part1 digplan = length inside
  where expanded = display $ snd $ foldl dig ((0,0),M.empty) digplan
        outside = fst $ converge (flood (asMap expanded)) (S.empty, S.singleton (0,0))
        inside = M.withoutKeys (asMap expanded) outside

fix :: Instruction -> BigDig
fix (_,hex) = (digitToInt $ last hex, fst $ head $ readHex (take 5 hex))

bigdig :: Pos -> BigDig -> Pos
bigdig (x,y) (0,width) = (x+width,y)
bigdig (x,y) (1,height) = (x,y+height)
bigdig (x,y) (2,width) = (x-width,y)
bigdig (x,y) (3,height) = (x,y-height)

offset (0,1) (x,y) = (x+1,y)
offset (0,3) (x,y) = (x,y)
offset (2,1) (x,y) = (x+1,y+1)
offset (2,3) (x,y) = (x,y+1)
offset (1,0) (x,y) = (x+1,y)
offset (1,2) (x,y) = (x+1,y+1)
offset (3,0) (x,y) = (x,y)
offset (3,2) (x,y) = (x,y+1)

part2 :: [Instruction] -> Int
part2 digplan = sum (zipWith det fixed (drop 1 $ cycle fixed)) `div` 2
  where plan = map fix digplan
        positions = scanl bigdig (0,0) plan
        turns = map fst plan
        corners = zip turns (rotate 1 turns)
        fixed = zipWith offset ((3,0):corners) positions
        det (a,b) (c,d) = (a * d) - (b * c)

main = do
    digplan <- fromJust . parseMaybe instructions <$> readFile "day18.txt"
    print $ part1 digplan
    print $ part2 digplan

display :: M.Map Pos String -> [String]
display grid = [ [ rep $ M.lookup (x,y) grid | x <- [(xMin-1)..(xMax+1)]] | y <- [(yMin-1)..(yMax+1)]]
  where rep (Just s) = '#'
        rep Nothing = '.'
        (xMax,yMax) = (maximum $ map fst $ M.keys grid, maximum $ map snd $ M.keys grid)
        (xMin,yMin) = (minimum $ map fst $ M.keys grid, minimum $ map snd $ M.keys grid)

parseMaybe :: ReadP a -> String -> Maybe a
parseMaybe parser input =
    case reverse $ readP_to_S parser input of
        [] -> Nothing
        ((result, _):_) -> Just result
