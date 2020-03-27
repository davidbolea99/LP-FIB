-- CUA 1
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


-- CUA 2

instance Functor Queue
    where
        fmap f (Queue s t) = (Queue (fmap f s) (fmap f t));

translation :: Num b => b -> Queue b -> Queue b
translation num cua = fmap (+ num) cua

queue2list :: Queue a -> [a]
queue2list (Queue s t) = s ++ (reverse t)

instance Applicative Queue
    where
        pure x = Queue [x] [] -- Operador que coge un valor y construye una queue con ese valor dentro
        (Queue fs ft) <*> (Queue es et) = Queue l []
            where
                l = [funcion elemento | funcion <- listaf, elemento <- listaq]
                listaf = fs ++ (reverse ft)
                listaq = es ++ (reverse et)

instance Monad Queue
    where
        return x    = Queue [x] []
        q >>= f     = Queue l [] -- empaqueta lista l en queue
            where
                l = (queue2list q) >>= (queue2list . f) -- la cola q se desempaqueta en la lista l y le aplicas la funcion f

kfilter :: (p -> Bool) -> Queue p -> Queue p
kfilter f q = do
    x <- q
    if (f x) then return x
    else (Queue [] [])