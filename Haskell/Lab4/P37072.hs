data Tree a = Node a (Tree a) (Tree a) | Empty deriving (Show)

size :: Tree a -> Int
size Empty = 0
size (Node _ ti td) = 1 + (size ti) + (size td)

height :: Tree a -> Int
height Empty = 0
height (Node _ ti td) = 1 + (max (height ti) (height td))

equal :: Eq a => Tree a -> Tree a -> Bool
equal Empty Empty = True
equal Empty (Node _ _ _) = False
equal (Node _ _ _) Empty = False
equal (Node x xl xr) (Node y yi yd) = (x == y) && (equal xl yi) && (equal xr yd)

preOrder :: Tree a -> [a]
preOrder Empty = []
preOrder (Node x xl xr) = [x] ++ (preOrder xl) ++ (preOrder xr)

postOrder :: Tree a -> [a]
postOrder Empty = []
postOrder (Node x xl xr) = (postOrder xl) ++ (postOrder xr) ++ [x]

inOrder :: Tree a -> [a]
inOrder Empty = []
inOrder (Node x xl xr) = (inOrder xl) ++ [x] ++ (inOrder xr)

breadthFirst :: Tree a -> [a]
breadthFirst t = breadthFirst' [t]
    where
        breadthFirst' :: [Tree a] -> [a]
        breadthFirst' [] = []
        breadthFirst' (Empty:xs) = breadthFirst' xs
        breadthFirst' ((Node n l r):xs) = n:(breadthFirst' (xs ++ [l,r]))
