import Data.Text (splitOn, pack, unpack)

waysToBeat duration record = length $ filter (> record ) distances
    where distances = map (\hold -> hold * (duration - hold)) [0..duration]

part1 :: [(Int,Int)] -> Int
part1 = product . map (uncurry waysToBeat)

parsePairs :: String -> [(Int, Int)]
parsePairs = uncurry zip . pairUp . map ints . lines
    where ints :: String -> [Int]
          ints = (map read . words . unpack . last) . splitOn (pack ":") . pack
          pairUp [a,b] = (a,b)

main = do
    races <- parsePairs <$> readFile "day06.txt"
    print $ part1 races