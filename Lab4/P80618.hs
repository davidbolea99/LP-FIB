data Queue a = Queue [a] [a]
     deriving (Show)

create :: Queue a
create = Queue [] []

push :: a -> Queue a -> Queue a
push x (Queue s t) = (Queue s (x:t))

pop :: Queue a -> Queue a
pop (Queue s t) = (Queue xs [])
    where
        (x:xs) = s ++ reverse(t)

top :: Queue a -> a
top (Queue [] t) = head $ reverse t
top (Queue (x:xs) t) = x

empty :: Queue a -> Bool
empty (Queue [] []) = True
empty (Queue s t) = False
