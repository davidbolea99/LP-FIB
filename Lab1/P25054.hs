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
    | otherwise     = (++) [x] (remove xs ys)


-- 6. Juntar un conjunto de listas en una sola lista (concat)

 flatten :: [[Int]] -> [Int]

 flatten (x:xs)
    | xs == []  = x
    | otherwise = (++) x (flatten xs)