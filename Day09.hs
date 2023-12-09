import Test.Hspec

derive :: [Int] -> [Int]
derive readings = zipWith (-) (tail readings) readings

derivatives :: [Int] -> [[Int]]
derivatives = takeWhile (not.all (0 ==)) . iterate derive

extrapolate :: [[Int]] -> [Int]
extrapolate = foldr1 addLast
    where addLast two one = two ++ [last one + last two]

artxepolate :: [[Int]] -> [Int]
artxepolate = foldr1 addFirst
    where addFirst two one = (head two - head one) : two

part1 :: [[Int]] -> Int
part1 = sum . map (last . extrapolate . derivatives)

part2 :: [[Int]] -> Int
part2 = sum . map (head . artxepolate . derivatives)

main = do
    readings <- map (map read . words) . lines <$> readFile "day09.txt"
    print $ part1 readings
    print $ part2 readings

test = hspec $ do
  describe "Derivatives" $ do
    it "Derive once" $ do
        derive [0,3,6,9,12,15] `shouldBe` [3,3,3,3,3]
    it "Derive many" $ do
        derivatives [0,3,6,9,12,15] `shouldBe` [[0,3,6,9,12,15],[3,3,3,3,3]]
        derivatives [1,3,6,10,15,21] `shouldBe` [[1,3,6,10,15,21],[2,3,4,5,6],[1,1,1,1]]
    it "Extrapolate" $ do
        extrapolate [[0,3,6,9,12,15],[3,3,3,3,3]] `shouldBe` [0,3,6,9,12,15,18]
        extrapolate [[1,3,6,10,15,21],[2,3,4,5,6],[1,1,1,1]] `shouldBe` [1,3,6,10,15,21,28]
        extrapolate (derivatives [10,13,16,21,30,45]) `shouldBe` [10,13,16,21,30,45,68]
    it "Extrapolate backwards" $ do
        artxepolate (derivatives [10,13,16,21,30,45]) `shouldBe` [5,10,13,16,21,30,45]

