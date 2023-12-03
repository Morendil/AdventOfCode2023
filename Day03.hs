import Data.Char
import Data.List
import Data.List.Extra
import qualified Data.Map as M

type Schematic = M.Map (Int,Int) Char
type PartNumber = (Int, [Int])

isPeriod = (==) '.'
isSpecial c = not (isDigit c) && not (isPeriod c)

offsets = map (\[a,b] -> (a,b)) $ sequence [[-1,0,1],[-1,0,1]]
neighbors :: PartNumber -> [(Int,Int)]
neighbors (y,xs) = nub $ concat [map (add (x,y)) offsets | x <- xs ]
    where add (x1,y1) (x2,y2) = (x1+x2,y1+y2)

toMap :: [String] -> Schematic
toMap grid@(one:_) = M.fromList [((x,y), grid !! y !! x) | x <- [0..length one-1], y <- [0..length grid-1]]

partNumbers :: [String] -> [PartNumber]
partNumbers = concat . zipWith (\n s -> map (n,) $ runs s) [0..]

isTagged :: Schematic -> PartNumber -> Bool
isTagged sch num = any (\loc -> isSpecial $ M.findWithDefault '.' loc sch) $ neighbors num

retrieve :: Schematic -> PartNumber -> Int
retrieve sch (y,xs) = read [M.findWithDefault '.' (x,y) sch | x <- xs]

runs :: String -> [[Int]]
runs line = map (map snd) $ filter (any (isDigit.fst)) $ groupOn (isDigit.fst) (zip line [0..])

main = do
    all <- lines <$> readFile "day03.txt"
    let schematic = toMap all
        parts = partNumbers all
        tagged = filter (isTagged schematic) parts
    print $ sum $ map (retrieve schematic) tagged