main = do
    line <- getContents
    let suma = show (sumatot $ words line)
    putStrLn suma
    return ()


sumatot :: [String] -> Integer
sumatot line = sumatot' (map read $ line :: [Integer])
    where
        sumatot' :: [Integer] -> Integer
        sumatot' int_list = foldl (+) 0 int_list