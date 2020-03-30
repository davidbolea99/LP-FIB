main = do
    line <- getLine
    if line /= "*" then do
        putStrLn $ process line
        main
    else
        return ()

process :: String -> String
process line = name ++ ": " ++ bmi
    where
        (name:xs) = words line
        (m:h:_) = map read $ xs :: [Float]
        bmi = bmi_calc m h

bmi_calc :: Float -> Float -> String
bmi_calc m h
    | n <= 18    = "underweight"
    | n <= 25    = "normal weight"
    | n <= 30    = "overweight"
    | n <= 40    = "obese"
    | otherwise    = "severely obese"
    where
        n = m/(h*h)
