data Queue a = Queue [a] [a]
     deriving (Show)


-- PART 1

create :: Queue a
create = Queue [] []

push :: a -> Queue a -> Queue a
push x (Queue s t) = (Queue s (x:t))

pop :: Queue a -> Queue a
pop (Queue [] t) = (Queue (drop 1 (reverse t)) [])
pop (Queue s t) = (Queue (drop 1 s) t)

top :: Queue a -> a
top (Queue [] t) = last t
top (Queue (x:xs) t) = x

empty :: Queue a -> Bool
empty (Queue [] []) = True
empty (Queue s t) = False


-- PART 2

instance Eq a => Eq (Queue a)
    where
        (Queue [] []) == (Queue [] []) = True
        q1 == q2
            | (empty q1) || (empty q2)  = False
            | (top q1) == (top q2)      = (pop q1) == (pop q2)
            | otherwise                 = False
