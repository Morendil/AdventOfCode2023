import Text.ParserCombinators.ReadP
import qualified Data.Map as M
import Data.Char (isAlpha)
import Data.Maybe (fromJust)

type Network = M.Map String (String, String)
type Directions = (String, Network)

ident = many1 (satisfy isAlpha)
pair = between (string "(") (string ")") $ (,) <$> ident <*> (string ", " *> ident)
directions :: ReadP Directions
directions = do
    path <- manyTill (satisfy isAlpha) (string "\n\n")
    pairs <- sepBy1 ((,) <$> ident <*> (string " = " *> pair)) (string "\n")
    return (path, M.fromList pairs)

follow :: Directions -> [String]
follow (path, nodes) = scanl takeTurn "AAA" (cycle path)
    where takeTurn node turn = (if turn == 'L' then fst else snd) $ fromJust $ M.lookup node nodes

part1 :: Directions -> Int
part1 = length . takeWhile ("ZZZ" /=) . follow

main = do
    instructions <- fromJust . parseMaybe directions <$> readFile "day08.txt"
    print $ part1 instructions


parseMaybe :: ReadP a -> String -> Maybe a
parseMaybe parser input =
    case reverse $ readP_to_S parser input of
        [] -> Nothing
        ((result, _):_) -> Just result