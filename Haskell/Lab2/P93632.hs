-- Comprobar si dos listas son iguales
eql :: [Int] -> [Int] -> Bool
eql x y = (length x == length y) && (and $ zipWith (==) x y)


-- Producto de los elementos de una lista entre ellos
prod :: [Int] -> Int
prod x = foldl (*) 1 x


-- Producto de los elementos pares de una lista entre ellos
prodOfEvens :: [Int] -> Int
prodOfEvens x = prod $ (filter even x)


-- Coger potencias de 2 en una lista
powersOf2 :: [Int]
powersOf2 = iterate (*2) 1


-- Producto escalar de dos listas
scalarProduct :: [Float] -> [Float] -> Float
scalarProduct x y = foldl (+) 0 $ zipWith (*) x y
