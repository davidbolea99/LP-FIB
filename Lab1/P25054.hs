-- 1. Longitud de una lista (version estupida)

myLength :: [Int] -> Int

myLength [] = 0
myLength (x:xs) = 1 + (myLength xs)


-- 2. Maximo de una lista

myMaximum :: [Int] -> Int

myMaximum (x:xs)
    | xs == []              = x
    | x > (myMaximum xs)    = x
    | otherwise             = (myMaximum xs)
    

-- 3. Average

average :: [Int] -> Float
average xs = (fromIntegral (sumaTodo xs)) / (fromIntegral (myLength xs))
    where
        sumaTodo :: [Int] -> Int
        sumaTodo [] = 0
        sumaTodo (x:xs) = x + (sumaTodo xs)


-- 4. Construir un palindromo
buildPalindrome :: [Int] -> [Int]

buildPalindrome xs = ((reverseList xs) ++ xs)
    where
        reverseList :: [Int] -> [Int]
        reverseList [] = []
        reverseList (y:ys) = ((reverseList ys) ++ [y])


-- 5. Elimina de la lista X los elementos de la lista de Y

remove :: [Int] -> [Int] -> [Int]

remove xs [] = xs
remove [] ys = []
remove (x:xs) ys
    | (elem x ys)   = (remove xs ys)
    | otherwise     = [x] ++ (remove xs ys)


-- 6. Juntar un conjunto de listas en una sola lista (concat)

flatten :: [[Int]] -> [Int]

flatten [] = []
flatten xs
    |(tail xs) == []  = (head xs)
    | otherwise         = (++) (head xs) (flatten (tail xs))

flattenPro :: [[Int]] -> [Int] -- Version utilizando la funcion incluida
flattenPro x = concat x


-- 7. Separa una lista en dos listas diferentes, una con los numeros pares y otra con los impares

oddsNevens :: [Int] -> ([Int],[Int])

oddsNevens list = oddsNevens' list [] []
    where
        oddsNevens' :: [Int] -> [Int] -> [Int] -> ([Int],[Int])
        oddsNevens' [] odd even = (odd, even)
        oddsNevens' (elem:others) odd even     
            | (mod elem 2) == 0  = oddsNevens' others odd (even ++ [elem])
            | otherwise          = oddsNevens' others (odd ++ [elem]) even

-- 8. Divisores primos de un natural > 0

primeDivisors :: Int -> [Int]

primeDivisors n = primeDivisors' [] 2
    where
        primeDivisors' :: [Int] -> Int -> [Int]
        primeDivisors' divisors d
            | d > n                             = divisors
            | (d == 2) && ((mod n 2) == 0)      = primeDivisors' (divisors ++ [2]) 3
            | (d == 2)                          = primeDivisors' divisors 3
            | ((mod n d) == 0) && (isPrime d)   = primeDivisors' (divisors ++ [d]) (d+2)
            | otherwise                         = primeDivisors' divisors (d+2)
            where
                isPrime :: Int -> Bool
                isPrime 0 = False
                isPrime 1 = False
                isPrime 2 = True
                isPrime n = isPrimeAux (floor (sqrt (fromIntegral n)))
                    where
                        isPrimeAux :: Int -> Bool
                        isPrimeAux d
                            | d == 1        = True
                            | mod n d == 0  = False
                            | otherwise     = isPrimeAux (d-1)