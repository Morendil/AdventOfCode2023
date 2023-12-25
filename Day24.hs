import Data.List.Extra (chunksOf, minimumOn, maximumOn)
import Linear.V3
import Linear.Metric
import Test.Hspec

type Stone = [[Integer]]
data RStone = RStone {px::Integer,py::Integer,pz::Integer,vx::Integer,vy::Integer,vz::Integer}
  deriving (Eq, Show)
type VStone = (V3 Integer, V3 Integer)

parse :: String -> Stone
parse = chunksOf 3 . map read . words . map dePunct
    where dePunct c | c `elem` ",@" = ' '
          dePunct c = c

asRec :: Stone -> RStone
asRec [[px,py,pz],[vx,vy,vz]] = RStone {px,py,pz,vx,vy,vz}

asVec :: Stone -> VStone
asVec [[px,py,pz],[vx,vy,vz]] = (V3 px py pz, V3 vx vy vz)

closest :: VStone -> VStone -> (V3 Double, V3 Double)
closest (p1,d1) (p2,d2) = (c1, c2)
  where n = cross d2 d1
        n2 = cross d2 n
        n1 = cross d1 n
        c1 = fmap fromIntegral p1 + (fromIntegral (dot (p2 - p1) n2) / fromIntegral (dot d1 n2)) * fmap fromIntegral d1
        c2 = fmap fromIntegral p2 + (fromIntegral (dot (p1 - p2) n1) / fromIntegral (dot d2 n1)) * fmap fromIntegral d2

coeffs :: [[Integer]] -> (Integer,Integer,Integer)
coeffs [[x,y,z],[vx,vy,vz]] = (vy,-vx,vx*y-vy*x)

intersect :: (Integer, Integer) -> [[Integer]] -> [[Integer]] -> Bool
intersect (cMin,cMax) s1 s2
        |  parallel = False
        |  fromIntegral vx1*(xMeet-fromIntegral x1) < 0 = False
        |  fromIntegral vy1*(yMeet-fromIntegral y1) < 0 = False
        |  fromIntegral vx2*(xMeet-fromIntegral x2) < 0 = False
        |  fromIntegral vy2*(yMeet-fromIntegral y2) < 0 = False
        |  xMeet >= fMin && xMeet <= fMax && yMeet >= fMin && yMeet <= fMax = True
        | otherwise = False
    where [[x1,y1,z1],[vx1,vy1,vz1]] = s1
          [[x2,y2,z2],[vx2,vy2,vz2]] = s2
          (a1,b1,c1) = coeffs s1
          (a2,b2,c2) = coeffs s2
          parallel = fromIntegral vx1 / fromIntegral vx2 == fromIntegral vy1 / fromIntegral vy2
          xMeet = fromIntegral (b1*c2 - b2*c1) / fromIntegral (a1*b2 - a2*b1)
          yMeet = fromIntegral (c1*a2 - c2*a1) / fromIntegral (a1*b2 - a2*b1)
          fMin = fromIntegral cMin
          fMax = fromIntegral cMax

crossWith :: Eq a => (a -> a -> b) -> [a] -> [b]
crossWith f list = concat [[f (list !! n) b | b <- drop (n+1) list] | n <- [0..length list-1]]

part1 :: [Stone] -> Int
part1 = length . filter id . crossWith (intersect (200000000000000,400000000000000))

main = do
    hail <- map parse . lines <$> readFile "day24_sample.txt"
    let vhail = map asVec hail
    putStrLn $ crossWith closest vhail

test = hspec $ do
  describe "Intersections" $ do
    it "In the test area" $ do
      intersect (7,27) [[19,13,30],[-2,1,-2]] [[18,19,22],[-1,-1,-2]] `shouldBe` True
    it "Parallel" $ do
      intersect (7,27) [[18,19,22],[-1,-1,-2]] [[20,25,34],[-2,-2,-4]] `shouldBe` False
    it "Outside the test area" $ do
      intersect (7,27) [[19,13,30],[-2,1,-2]] [[12,31,28],[-1,-2,-1]] `shouldBe` False
    it "In the past for one" $ do
      intersect (7,27) [[19,13,30],[-2,1,-2]] [[20,19,15],[1,-5,-3]] `shouldBe` False
    it "In the past for both" $ do
      intersect (7,27) [[12,31,28],[-1,-2,-1]] [[20,19,15],[1,-5,-3]] `shouldBe` False
