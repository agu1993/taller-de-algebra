factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n-1)

fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

parteEntera :: Float -> Int
parteEntera n | n < 1 = 0
              | n >= 1 = 1 + parteEntera (n - 1)

multiploDe3 :: Int -> Bool
multiploDe3 n | n <= 0 = True
              | n == 1 || n == 2 = False
              | n > 2 = multiploDe3 (n - 3)

sumarImpares :: Int -> Int
sumarImpares 1 = 1
sumarImpares n = n * 2 - 1 + sumarImpares (n - 1)

medioFact :: Int -> Int
medioFact 0 = 1
medioFact 1 = 1
medioFact n = n * medioFact (n - 2)

unidad :: Int -> Int
unidad n = mod n 10

sumarDigitos :: Int -> Int
sumarDigitos n | n < 10 = n
               | n >= 10 = unidad n + sumarDigitos (div n 10)

digitosIguales :: Int -> Bool
digitosIguales n | n < 10 = True
                 | n >= 10 = unidad n == unidad (div n 10) && digitosIguales (div n 10)

