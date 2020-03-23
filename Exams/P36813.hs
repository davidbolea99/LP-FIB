import Data.List

degree :: Eq a => [(a, a)] -> a -> Int
degree [] _ = 0
degree (e:edges) x
    | contain e x   = 1 + (degree edges x)
    | otherwise     = degree edges x
    where
        contain :: Eq a => (a, a) -> a -> Bool
        contain a b = (fst a) == b || (snd a) == b

-------------------------------------------------------------------------

degree' :: Eq a => [(a, a)] -> a -> Int
degree' [] _ = 0;
degree' lista x = foldl (+) 0 (map (contain') lista)
    where
        contain' y
            | (fst y) == x || (snd y) == x  = 1
            | otherwise                     = 0

-------------------------------------------------------------------------

neighbors :: Ord a => [(a, a)] -> a -> [a]

neighbors [] _ = []
neighbors list v = sort $ neighbors' list
    where
        neighbors' [] = []
        neighbors' (x:xs)
            | first == v    = (second:(neighbors' xs))
            | second == v   = (first:(neighbors' xs))
            | otherwise     = neighbors' xs
            where
                first = fst x
                second = snd x