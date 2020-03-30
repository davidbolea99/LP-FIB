flatten :: [[Int]] -> [Int]
flatten x = foldl (++) [] x


myLength :: String -> Int
myLength x = foldl (+) 0 $ map (const 1) x


 -- version con foldr
myReverse :: [Int] -> [Int]
myReverse n = foldr (\x acc -> acc ++ [x]) [] n

 -- version con foldl
myReverseL :: [Int] -> [Int]
myReverseL n = foldl (\acc x -> x : acc) [] n

countIn :: [[Int]] -> Int -> [Int]
countIn n p = map (\x -> length $ filter (==p) x) n

firstWord :: String -> String
firstWord s = takeWhile (/= ' ') (dropWhile (== ' ') s)
