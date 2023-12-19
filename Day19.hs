import Text.ParserCombinators.ReadP
import Data.Char (isNumber, isAlpha, isHexDigit, digitToInt, isAlphaNum)
import Data.Maybe (fromJust, mapMaybe)
import Data.List.HT (takeUntil)
import qualified Data.Map as M

type Part = M.Map String Int
type Flow = (String,[Rule])
type System = M.Map String [Rule]
data Op = More | Less deriving (Eq, Show)
data Rule = Condition String Op Int String | Go String deriving (Eq, Show)

rule :: ReadP Rule
rule = choice [conditional, goto]
conditional :: ReadP Rule
conditional = do
    name <- many1 (satisfy isAlphaNum)
    op <- choice [More <$ char '>', Less <$ char '<']
    arg <- number
    string ":"
    decision <- many1 (satisfy isAlphaNum)
    return $ Condition name op arg decision
goto :: ReadP Rule
goto = Go <$> many1 (satisfy isAlphaNum)
flow :: ReadP Flow
flow = do
    name <- many1 (satisfy isAlphaNum)
    rules <- between (string "{") (string "}") (sepBy1 rule (string ","))
    return (name, rules)
prop :: ReadP (String, Int)
prop = (,) <$> many1 (satisfy isAlphaNum) <*> (string "=" *> number)
part :: ReadP Part
part = M.fromList <$> between (string "{") (string "}") (sepBy1 prop (string ","))

program :: ReadP (System, [Part])
program = do
    rules <- sepBy1 flow (string "\n")
    string "\n\n"
    parts <- sepBy1 part (string "\n")
    return (M.fromList rules, parts)

number = read <$> many1 (satisfy isNumber)

decide :: System -> Part -> Bool
decide system part = (==)"A" $ last $ takeUntil (`elem` ["A","R"]) $ iterate next "in"
    where next = step system part

step :: System -> Part -> String -> String
step flow part from = head $ mapMaybe (`apply` part) rules
    where rules = fromJust $ M.lookup from flow

apply :: Rule -> Part -> Maybe String
apply (Go s) _ = Just s
apply (Condition prop op arg next) part = if cmp op val arg then Just next else Nothing
    where val = fromJust $ M.lookup prop part
          cmp More = (>)
          cmp Less = (<)

part1 :: System -> [Part] -> Int
part1 system parts = sum $ map sum $ filter (decide system) parts

main = do
    (system, parts) <- fromJust . parseMaybe program <$> readFile "day19.txt"
    print $ part1 system parts

parseMaybe :: ReadP a -> String -> Maybe a
parseMaybe parser input =
    case reverse $ readP_to_S parser input of
        [] -> Nothing
        ((result, _):_) -> Just result
