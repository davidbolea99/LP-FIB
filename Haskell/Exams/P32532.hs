divisors :: Int -> [Int]
divisors n = [d | d <- [1..n], (mod n d) == 0]

nbDivisors :: Int -> Int
nbDivisors = length . divisors

moltCompost :: Int -> Bool
moltCompost 1 = True
moltCompost n = and [me > (nbDivisors x) | x <- [1..n-1]]
    where
        me = nbDivisors n