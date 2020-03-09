data Expr = Val Int | Add Expr Expr | Sub Expr Expr | Mul Expr Expr | Div Expr Expr

eval1 :: Expr -> Int
eval1 (Val x) = x
eval1 (Add x y) = (eval1 x) + (eval1 y)
eval1 (Sub x y) = (eval1 x) - (eval1 y)
eval1 (Mul x y) = (eval1 x) * (eval1 y)
eval1 (Div x y) = (div (eval1 x) (eval1 y))

eval2 :: Expr -> Maybe Int

eval2 (Val x) = Just x

eval2 (Add e1 e2) = do
    x <- eval2 e1
    y <- eval2 e2
    return (x + y)

eval2 (Sub e1 e2) = do
    x <- eval2 e1
    y <- eval2 e2
    return (x - y)

eval2 (Mul e1 e2) = do
    x <- eval2 e1
    y <- eval2 e2
    return (x * y)

eval2 (Div e1 e2) = do
    x <- eval2 e1
    y <- eval2 e2
    if y == 0 then Nothing
    else return (div x y)



eval3 :: Expr -> Either String Int

eval3 (Val x) = Right x

eval3 (Add e1 e2) = do
    x <- eval3 e1
    y <- eval3 e2
    Right (x + y)

eval3 (Sub e1 e2) = do
    x <- eval3 e1
    y <- eval3 e2
    Right (x - y)

eval3 (Mul e1 e2) = do
    x <- eval3 e1
    y <- eval3 e2
    Right (x * y)

eval3 (Div e1 e2) = do
    x <- eval3 e1
    y <- eval3 e2
    if y == 0 then Left "div0"
    else Right (div x y)