main = do
    c <- getChar
    if c == 'A' || c == 'a' then putStrLn "Hello!"
    else putStrLn "Bye!"