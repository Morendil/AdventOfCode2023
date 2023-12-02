import Text.ParserCombinators.ReadP
import Data.Char (isNumber)
import Data.Maybe (fromJust)
import qualified Data.Map as M

type Game = (Int, [Sample])
type Sample = M.Map String Int

number = read <$> many1 (satisfy isNumber)
sample = M.fromList <$> sepBy1 draw (string ", ")
draw = flip (,) <$> number <*> (string " " *> choice (map string ["red","green","blue"]))
game = (,) <$> (string "Game " *> number) <*> (string ": " *> sepBy1 sample (string "; "))
games :: ReadP [Game]
games = sepBy1 game (string "\n")

parseMaybe :: ReadP a -> String -> Maybe a
parseMaybe parser input =
    case reverse $ readP_to_S parser input of
        [] -> Nothing
        ((result, _):_) -> Just result

possible :: Sample -> Game -> Bool
possible bag (_, samples) = all plausible samples
    where plausible sample = all (\(color,count) -> M.findWithDefault 0 color sample <= count) (M.toList bag)

part1 :: [Game] -> Int
part1 = sum . map fst . filter (possible $ M.fromList [("red",12),("green",13),("blue",14)])

main = do
    games <- fromJust . parseMaybe games <$> readFile "day02.txt"
    print $ part1 games