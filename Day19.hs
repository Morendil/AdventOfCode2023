import Text.ParserCombinators.ReadP
import Data.Char (isNumber, isAlpha, isHexDigit, digitToInt, isAlphaNum)
import Data.Maybe (fromJust, mapMaybe, catMaybes)
import Data.List.HT (takeUntil)
import Data.List (nub)
import qualified Data.Map as M
import qualified Data.Set as S

import Test.Hspec

type Part = M.Map String Int
type Flow = (String,[Rule])
type System = M.Map String [Rule]
data Op = More | Less deriving (Eq, Show, Ord)
data Rule = Condition String Op Int String | Go String deriving (Eq, Show, Ord)
type Quantum = M.Map String (Int,Int)

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
decide system part = (==) "A" $ last $ takeUntil (`elem` ["A","R"]) $ iterate next "in"
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

label :: Rule -> Quantum -> (String, Quantum)
label r = (ruleName r,)

ruleName :: Rule -> String
ruleName (Go s) = s
ruleName (Condition _ _ _ s) = s

split :: [Rule] -> Quantum -> [(String,Quantum)]
-- Silly special case where all rules have the same destination
split rules q | let l = nub (map ruleName rules), length l == 1 = [(head l, q)]
split [r1,r2] q = catMaybes [label r1 <$> pass r1 q, label r2 <$> chop r1 q]
split [r1,r2,r3] q = catMaybes [c1, c2, c3]
    where c1 = label r1 <$> pass r1 q
          c2 = label r2 <$> (pass r2 =<< chop r1 q)
          c3 = label r3 <$> (chop r2 =<< chop r1 q)
split [r1,r2,r3,r4] q = catMaybes [c1, c2, c3, c4]
    where c1 = label r1 <$> pass r1 q
          c2 = label r2 <$> (pass r2 =<< chop r1 q)
          c3 = label r3 <$> (pass r3 =<< chop r2 =<< chop r1 q)
          c4 = label r4 <$> (chop r3 =<< chop r2 =<< chop r1 q)

quantumStep :: System -> [(String,Quantum)] -> [(String,Quantum)]
quantumStep system states = arrived ++ concatMap splitByRule active
  where splitByRule (rule,q) = split (fromJust $ M.lookup rule system) q
        active = filter (not .(`elem` ["A","R"]) .fst) states
        arrived = filter ((==)"A".fst) states

pass :: Rule -> Quantum -> Maybe Quantum
pass (Go s) q = Just q
pass (Condition prop Less arg next) q = if vmin < arg then passIt else Nothing
    where (vmin,vmax) = fromJust $ M.lookup prop q
          passIt = Just $ M.insert prop (vmin,min (arg-1) vmax) q
pass (Condition prop More arg next) q = if vmax > arg then passIt else Nothing
    where (vmin,vmax) = fromJust $ M.lookup prop q
          passIt = Just $ M.insert prop (max (arg+1) vmin,vmax) q

chop :: Rule -> Quantum -> Maybe Quantum
chop (Condition prop Less arg next) = pass (Condition prop More (arg-1) next)
chop (Condition prop More arg next) = pass (Condition prop Less (arg+1) next)

part1 :: System -> [Part] -> Int
part1 system parts = sum $ map sum $ filter (decide system) parts

converge :: Eq a => (a -> a) -> a -> a
converge = until =<< ((==) =<<)

part2 :: System -> Integer
part2 system = sum $ map value final
  where init = [("in",M.fromList [("x",(1,4000)),("m",(1,4000)),("a",(1,4000)),("s",(1,4000))])]
        final = converge (quantumStep system) init
        value :: (String, Quantum) -> Integer
        value (s,q) = product $ M.map (\(a,b)-> fromIntegral $ b-a+1) q

main = do
    (system, parts) <- fromJust . parseMaybe program <$> readFile "day19.txt"
    print $ part1 system parts
    print $ part2 system

parseMaybe :: ReadP a -> String -> Maybe a
parseMaybe parser input =
    case reverse $ readP_to_S parser input of
        [] -> Nothing
        ((result, _):_) -> Just result

test = hspec $ do
  describe "Quantum splits" $ do
    it "Pass, incompatible, less" $ do
      let org = M.fromList [("x",(1,4000)),("m",(1,4000)),("a",(1,4000)),("s",(1351,4000))]
          aRule = fromJust $ parseMaybe rule "s<1351:px"
      pass aRule org `shouldBe` Nothing
    it "Pass, compatible, less" $ do
      let org = M.fromList [("x",(1,4000)),("m",(1,4000)),("a",(1,4000)),("s",(1350,4000))]
          aRule = fromJust $ parseMaybe rule "s<1351:px"
      pass aRule org `shouldBe` Just (M.insert "s" (1350,1350) org)
    it "Pass, compatible, already less" $ do
      let org = M.fromList [("x",(1,4000)),("m",(1,4000)),("a",(1,4000)),("s",(1300,1300))]
          aRule = fromJust $ parseMaybe rule "s<1351:px"
      pass aRule org `shouldBe` Just (M.insert "s" (1300,1300) org)
    it "Pass, incompatible, more" $ do
      let org = M.fromList [("x",(1,4000)),("m",(1,4000)),("a",(1,4000)),("s",(1,1351))]
          aRule = fromJust $ parseMaybe rule "s>1351:px"
      pass aRule org `shouldBe` Nothing
    it "Pass, compatible, more" $ do
      let org = M.fromList [("x",(1,4000)),("m",(1,4000)),("a",(1,4000)),("s",(1,4000))]
          aRule = fromJust $ parseMaybe rule "s>1351:px"
      pass aRule org `shouldBe` Just (M.insert "s" (1352,4000) org)
    it "Pass, compatible, already more" $ do
      let org = M.fromList [("x",(1,4000)),("m",(1,4000)),("a",(1,4000)),("s",(2000,4000))]
          aRule = fromJust $ parseMaybe rule "s>1351:px"
      pass aRule org `shouldBe` Just (M.insert "s" (2000,4000) org)
    it "Chop, incompatible, less" $ do
      let org = M.fromList [("x",(1,4000)),("m",(1,4000)),("a",(1,4000)),("s",(1351,4000))]
          aRule = fromJust $ parseMaybe rule "s<1351:px"
      chop aRule org `shouldBe` Just org
    it "Chop, compatible, less" $ do
      let org = M.fromList [("x",(1,4000)),("m",(1,4000)),("a",(1,4000)),("s",(1,4000))]
          aRule = fromJust $ parseMaybe rule "s<1351:px"
      chop aRule org `shouldBe` Just (M.insert "s" (1351,4000) org)
    it "Two rules" $ do
      let org = M.fromList [("x",(1,4000)),("m",(1,4000)),("a",(1,4000)),("s",(1,4000))]
          aFlow = fromJust $ parseMaybe flow "in{s<1351:px,qqz}"
      split (snd aFlow) org `shouldBe` [("px",M.insert "s" (1,1350) org),("qqz",M.insert "s" (1351,4000) org)]
