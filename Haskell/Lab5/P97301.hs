fizzBuzz :: [Either Int String]
fizzBuzz = map f (iterate (+1) 0)
    where
        f :: Int -> Either Int String
        f n
            | (mod n 15) == 0   = Right "FizzBuzz"
            | (mod n 3) == 0    = Right "Fizz"
            | (mod n 5) == 0    = Right "Buzz"
            | otherwise         = Left n
