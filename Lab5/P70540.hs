data Expr = Val Int | Add Expr Expr | Sub Expr Expr | Mul Expr Expr | Div Expr Expr

eval1 :: Expr -> Int
eval1 (Val x) = x
eval1 (Add x y) = (eval1 x) + (eval1 y)
eval1 (Sub x y) = (eval1 x) - (eval1 y)
eval1 (Mul x y) = (eval1 x) * (eval1 y)
eval1 (Div x y) = (div (eval1 x) (eval1 y))

eval2 :: Expr -> Maybe Int

eval2 (Val x) = (Just x)
eval2 (Add x y) = Just ((eval2 x) + (eval2 y))
eval2 (Sub x y) = Just ((eval2 x) - (eval2 y))
eval2 (Mul x y) = Just ((eval2 x) * (eval2 y))
eval2 (Div x y)
    | y == 0    = Nothing
    | otherwise = (Just (div x y))