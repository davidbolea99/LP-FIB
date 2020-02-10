-- Valor Absoluto

absValue :: Int -> Int

absValue n
  | n >= 0    = n
  | otherwise = (*) n (-1)



-- Potenciacion

power :: Int -> Int -> Int

power b p
  | p < 0     = div 1 (power b (absValue p))
  | p == 0    = 1
  | otherwise = b * (power b (p - 1))

  

-- Numero Primo

            
isPrime :: Int -> Bool
isPrime 0 = False
isPrime 1 = False
isPrime 2 = True
isPrime n = isPrimeAux (floor (sqrt (fromIntegral n)))
    where
        isPrimeAux :: Int -> Bool
        isPrimeAux d
            | d == 1        = True
            | mod n d == 0  = False
            | otherwise     = isPrimeAux (d-1)

-- slowFib

slowFib :: Int -> Int
slowFib 0 = 0
slowFib 1 = 1
slowFib n = (slowFib (n-1)) + (slowFib (n-2))
