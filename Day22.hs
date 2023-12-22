import Data.List.Extra (chunksOf, findIndices)
import Data.List ( transpose, tails, inits, sortOn, (\\), partition )
import qualified Data.Map as M
import Test.Hspec
import Data.Ord (Down(..))
import Control.Monad.State (State, runState, get, put)

type Brick = [[Int]]
type SupportedBy = M.Map Int [Int]

parse :: String -> Brick
parse = chunksOf 3 . map read . words . map dePunct
    where dePunct c | c `elem` ",~" = ' '
          dePunct c = c

overlap :: [Int] -> [Int] -> Bool
overlap [x1,x2] [y1,y2] = x1 <= y2 && y1 <= x2
overlap _ _ = error "?"

overlap2d :: Brick -> Brick -> Bool
overlap2d a b = and $ zipWith overlap (take 2 a) (take 2 b)

settle  :: [Brick] -> [Brick]
settle = fst . settle'

settle' :: [Brick] -> ([Brick], SupportedBy)
settle' = go M.empty []
  where go sup settled [] = (reverse settled, sup)
        go sup settled (brick:bricks) = go (M.insert index supports sup) (settleOne:settled) bricks
          where settleOne = dropTo (zMax+1) brick
                below = sortOn (Down . \xyz -> xyz !! 2 !! 1) $ filter (overlap2d brick) settled
                zMax = if null below then 0 else head below !! 2 !! 1
                supports = findIndices (\xyz -> xyz !! 2 !! 1 == zMax && overlap2d xyz brick) (reverse settled)
                index = length settled
        dropTo z [xs,ys,[z1,z2]] = [xs,ys,[z,z+(z2-z1)]]

key :: SupportedBy -> Int -> Bool
key supports n = [n] `elem` M.elems supports

-- Would be better memoized…
chain :: SupportedBy -> Int -> Int
chain supports n = go (M.filter (not.null) supports) [n]
    where go supports _ | null supports = 0
          go supports remove = if null fell then 0 else length fell + go remain (M.keys fell)
            where supports' = M.map (\\ remove) supports
                  (fell, remain) = M.partition null supports'

part1 :: [Brick] -> Int
part1 bricks = length $ filter (not . key supports) [0..length settled-1]
  where (settled, supports) = settle' bricks

display :: String -> Brick -> String
display c [[x1,x2],[y1,y2],[z1,z2]] = "       color(\""++c++"\") translate(["++show x1++","++show y1++","++show z1++"]) cube(["++show (x2-x1+1)++","++show (y2-y1+1)++","++show (z2-z1+1)++"]);"

main = do
    colors <- lines <$> readFile "colors.txt"
    bricks <- sortOn (minimum . \b -> b!! 2) . map (transpose . parse) . lines <$> readFile "day22.txt"
    let (settled, supports) = settle' bricks
    print $ part1 bricks
    print $ map (chain supports) [0..length settled-1]
    -- print $ filter (key supports) [0..50]
    -- putStrLn $ unlines $ zipWith display (cycle colors) settled
    -- putStrLn $ unlines $ zipWith display (cycle colors)  $ take 113 bricks
    -- putStrLn $ unlines $ map display $ settle bricks

test = hspec $ do
  describe "Settling" $ do
    it "Settle one on the ground" $ do
      settle [[[1,1],[0,2],[1,1]]] `shouldBe` [[[1,1],[0,2],[1,1]]]
    it "Settle one in the air" $ do
      settle [[[1,1],[0,2],[3,3]]] `shouldBe` [[[1,1],[0,2],[1,1]]]
    it "Settle two supported" $ do
      settle [[[1,1],[0,2],[1,1]],[[0,2],[0,0],[2,2]]] `shouldBe` [[[1,1],[0,2],[1,1]],[[0,2],[0,0],[2,2]]]
    it "Settle three, one goes down" $ do
      settle [[[1,1],[0,2],[1,1]],[[0,2],[0,0],[2,2]],[[0,2],[2,2],[3,3]]] `shouldBe` [[[1,1],[0,2],[1,1]],[[0,2],[0,0],[2,2]],[[0,2],[2,2],[2,2]]]
    it "Settle the entire example" $ do
      let up = [[[1,1],[0,2],[1,1]],[[0,2],[0,0],[2,2]],[[0,2],[2,2],[3,3]],[[0,0],[0,2],[4,4]],[[2,2],[0,2],[5,5]],[[0,2],[1,1],[6,6]],[[1,1],[1,1],[8,9]]]
          down = [[[1,1],[0,2],[1,1]],[[0,2],[0,0],[2,2]],[[0,2],[2,2],[2,2]],[[0,0],[0,2],[3,3]],[[2,2],[0,2],[3,3]],[[0,2],[1,1],[4,4]],[[1,1],[1,1],[5,6]]]
      settle up `shouldBe` down