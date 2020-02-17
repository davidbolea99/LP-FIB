-- emulates map using comprehension lists.

myMap :: (a -> b) -> [a] -> [b]

myMap f l = [(f x) | x <- l]



-- emulates zipWith using comprehension lists and zip

myFilter :: (a -> Bool) -> [a] -> [a] 

myFilter p l = [x | x <- l, (p x)]



-- mulates zipWith using comprehension lists and zip

--myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]



-- given two lists of integers, returns the list that
-- pairs the elements if the element of the second list
-- divides the one in the first list

--thingify :: [Int] -> [Int] -> [(Int, Int)]


-- given a non-null natural number, generates the ordered
-- list with all its factors (non necessaryly primes).

--factors :: Int -> [Int]
