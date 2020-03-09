fizzBuzz :: [Either Int String]
fizzBuzz = iterate f 0
    where
        f :: Int -> Int
        f x = do
            [Right