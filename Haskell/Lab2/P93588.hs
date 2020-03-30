-- emulates map using comprehension lists.

myMap :: (a -> b) -> [a] -> [b]

myMap f l = [(f x) | x <- l]



-- emulates zipWith using comprehension lists and zip

myFilter :: (a -> Bool) -> [a] -> [a] 

myFilter p l = [x | x <- l, (p x)]



-- mulates zipWith using comprehension lists and zip

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]

myZipWith f l1 l2 = [(f x y) | (x,y) <- (zip l1 l2)]    --  <--- PREGUNTAR !!!!!!!!!!!!!



-- given two lists of integers, returns the list that
-- pairs the elements if the element of the second list
-- divides the one in the first list

thingify :: [Int] -> [Int] -> [(Int, Int)]

thingify l1 l2 = [(x,y) | x <- l1, y <- l2, ((mod x y) == 0)]



-- given a non-null natural number, generates the ordered
-- list with all its factors (non necessaryly primes).

factors :: Int -> [Int]

factors n = [x | x <- [1..n], (mod n x) == 0]
