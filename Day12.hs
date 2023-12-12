import Data.Text (pack, unpack, splitOn)
import Data.List (group, inits, intercalate, nub)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe (fromJust)
import Test.Hspec

type Record = (String, [Int])
type Group = (Int, Char)
data Current = Working | Broken Int deriving (Eq, Show, Ord)
type Entry = (Current, [Int])
type State = M.Map Entry Int

parse :: String -> Record
parse s = (springs, runLengths)
    where [springs, runs] = words s
          runLengths = map (read . unpack) $ splitOn (pack ",") $ pack runs

bruteforce :: Record -> Int
bruteforce (springs, runLengths) = length $ filter (match runLengths) candidates
    where match lengths candidate = lengths == map length (filter ('#' `elem`) $ group candidate)
          candidates = mapM (\c -> if c == '?' then ['.','#'] else [c]) springs

arrangements :: Record -> Int
arrangements = sum . cleanup . consume

consume :: Record -> State
consume (springs, runLengths) = foldl advance init springs
  where init :: State
        init = M.singleton (Working, runLengths) 1

cleanup :: State -> State
cleanup = (`M.restrictKeys` S.fromList [(Working, []),(Broken 0,[])])

advance :: State -> Char -> State
advance state c = M.filter (>0) $ foldl (flip ($)) state $ map (step c) (M.assocs state)

add val = M.alter (Just . maybe val (+val))
dec val = M.update (Just . (\old -> old - val))

step :: Char -> (Entry, Int) -> State -> State
step '#' ((Working, []),val) s            = dec val (Working, []) s
step '?' ((Working, runs@(n:rest)),val) s = add val (Broken (n-1), rest) s
step '#' ((Working, runs@(n:rest)),val) s = dec val (Working, runs) $ add val (Broken (n-1), rest) s
step  _  ((Working, runs),_)            s = s
step '.' ((Broken n, runs),val) s | n > 0 = dec val (Broken n, runs) s
step  _  ((Broken n, runs),val) s | n > 0 = dec val (Broken n, runs) $ add val (Broken (n-1), runs) s
step '#' ((Broken 0, runs),val)         s = dec val (Broken 0, runs) s
step  _  ((Broken 0, runs),val)         s = dec val (Broken 0, runs) $ add val (Working, runs) s

part1 :: [Record] -> Int
part1 = sum . map arrangements

part2 :: [Record] -> Int
part2 = sum . map (arrangements . expand 5)

expand :: Int -> Record -> Record
expand n (springs, runs) = (intercalate "?" $ replicate n springs, concat $ replicate n runs)

main = do
    records <- map parse . lines <$> readFile "day12.txt"
    print $ part1 records
    print $ part2 records

test = hspec $ do
  describe "Arrangements" $ do
    it "Simple arrangements" $ do
      arrangements ("#",[1]) `shouldBe` 1
      arrangements ("?",[1]) `shouldBe` 1
      arrangements (".",[1]) `shouldBe` 0
      arrangements ("##",[1]) `shouldBe` 0
      arrangements ("???",[1]) `shouldBe` 3
      arrangements ("???",[2]) `shouldBe` 2
      arrangements ("??",[1]) `shouldBe` 2
      arrangements ("???",[1,1]) `shouldBe` 1
      arrangements ("???#",[1,1]) `shouldBe` 2
      arrangements ("???????",[1]) `shouldBe` 7
    it "Harder arrangements" $ do
      arrangements ("?#?##??#?#?#????",[12,1]) `shouldBe` 5
      arrangements ("?????.?????#????????",[1,1,5,3]) `shouldBe` 195