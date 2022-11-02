-- EJERCICIO 1: sonCoprimos
sonCoprimos :: Integer -> Integer -> Bool
sonCoprimos n m = mcd n m == 1 || mcd n m == -1

-- EJERCICIO 2: es2Pseudoprimo
es2Pseudoprimo :: Integer -> Bool
es2Pseudoprimo n = esAPseudoprimo 2 n 

-- EJERCICIO 3: cantidad3Pseudoprimos
cantidad3Pseudoprimos :: Integer -> Integer
cantidad3Pseudoprimos n | n == 1 = 0
                        | es3Pseudoprimo n = 1 + cantidad3Pseudoprimos (n - 1)
                        | otherwise = cantidad3Pseudoprimos (n - 1)

-- EJERCICIO 4: kesimo2y3Pseudoprimo
kesimo2y3Pseudoprimo :: Integer -> Integer
kesimo2y3Pseudoprimo n = nproximos2y3Pseudoprimos 1 n 

-- EJERCICIO 5: esCarmichael
esCarmichael :: Integer -> Bool
esCarmichael n = esCarmichaelAux n (n - 1)

-- Funciones Auxiliares --

es2y3Pseudoprimo :: Integer -> Bool
es2y3Pseudoprimo n = es2Pseudoprimo n && es3Pseudoprimo n

proximo2y3Pseudoprimo :: Integer -> Integer
proximo2y3Pseudoprimo n | es2y3Pseudoprimo (n + 1) = n + 1
                        | otherwise = proximo2y3Pseudoprimo (n + 1)

nproximos2y3Pseudoprimos :: Integer -> Integer -> Integer
nproximos2y3Pseudoprimos n m | m == 1 = proximo2y3Pseudoprimo n
                             | otherwise = nproximos2y3Pseudoprimos (proximo2y3Pseudoprimo n) (m - 1)

es3Pseudoprimo :: Integer -> Bool
es3Pseudoprimo n = esAPseudoprimo 3 n

mcd :: Integer -> Integer -> Integer
mcd n m | mod n m == 0 = m
        | otherwise = mcd m (mod n m)

mincd :: Integer -> Integer -> Integer
mincd n m | mod n m == 0 = m
          | otherwise = mincd n (m - 1)

esPrimo :: Integer -> Bool
esPrimo 1 = False
esPrimo n = mincd n (n - 1) == 1

esAPseudoprimo :: Integer -> Integer -> Bool
esAPseudoprimo n m = mod (n^(m - 1) - 1) m == 0 && not (esPrimo m)

esCarmichaelAux :: Integer -> Integer -> Bool
esCarmichaelAux n m | m == 1 = True
                  | not (sonCoprimos n m) = esCarmichaelAux n (m - 1)
                  | esAPseudoprimo m n = esCarmichaelAux n (m - 1)
                  | otherwise = False

