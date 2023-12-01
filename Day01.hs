import Data.Char

digits :: String -> String
digits = filter isDigit

firstAndLast :: [a] -> [a]
firstAndLast l = head l : [last l]

value :: String -> Int
value = read . firstAndLast . digits

main = do
    calibration <- map value . lines <$> readFile "day01.txt"
    print $ sum calibration
