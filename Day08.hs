import Text.ParserCombinators.ReadP
import qualified Data.Map as M
import qualified Data.Set as S
import Data.List (nub, findIndex)
import Data.List.HT (takeUntil)
import Data.Char (isAlphaNum)
import Data.Maybe (fromJust, mapMaybe)

type Network = M.Map String (String, String)
type Directions = (String, Network)

ident = many1 (satisfy isAlphaNum)
pair = between (string "(") (string ")") $ (,) <$> ident <*> (string ", " *> ident)
directions :: ReadP Directions
directions = do
    path <- manyTill (satisfy isAlphaNum) (string "\n\n")
    pairs <- sepBy1 ((,) <$> ident <*> (string " = " *> pair)) (string "\n")
    return (path, M.fromList pairs)

follow :: String -> Directions -> [String]
follow start (path, nodes) = scanl takeTurn start (cycle path)
    where takeTurn node turn = (if turn == 'L' then fst else snd) $ fromJust $ M.lookup node nodes

type Table = M.Map String [String]
table :: Directions -> Table
table instructions@(path, nodes) = M.fromList trails
    where trails = [(head trail, trail) | n <-  M.keys nodes, last n /= 'Z', let trail = take (length path + 1) $ follow n instructions]

followFast :: Table -> String -> [String]
followFast table start = takeUntil (any ((==) 'Z'.last). fromJust . (`M.lookup`table)) $ iterate next start
    where next node = last $ fromJust $ M.lookup node table

countSteps :: Directions -> String -> Integer
countSteps instructions start = (fromIntegral pathLen * fromIntegral count) + fromIntegral lastMile
        where trails = table instructions
              stops = followFast trails start
              count = length stops - 1
              pathLen = length $ fst instructions
              lastMile = fromJust $ findIndex ((==) 'Z'.last) $ fromJust $ M.lookup (last stops) trails

part1 :: Directions -> Integer
part1 = (`countSteps` "AAA")

part2 :: Directions -> Integer
part2 instructions@(path,nodes) = foldl1 lcm $ map (countSteps instructions) starts
    where starts = filter ((==) 'A'.last) $ M.keys nodes

main = do
    instructions <- fromJust . parseMaybe directions <$> readFile "day08.txt"
    print $ part1 instructions
    print $ part2 instructions


parseMaybe :: ReadP a -> String -> Maybe a
parseMaybe parser input =
    case reverse $ readP_to_S parser input of
        [] -> Nothing
        ((result, _):_) -> Just result