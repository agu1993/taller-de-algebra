sumatoriaGauss :: Int -> Int
sumatoriaGauss 1 = 1
sumatoriaGauss n = n + sumatoriaGauss (n-1)

sumatoriaGaussCerrada :: Int -> Int
sumatoriaGaussCerrada n = div (n * (n + 1)) 2 

f1 :: Int -> Int
f1 0 = 1
f1 n = 2 ^ n + f1 (n - 1)

f2 :: Int -> Int -> Int
f2 1 q = q
f2 n q = q ^ n + f2 (n - 1) q

f3 :: Int -> Int -> Int
f3 0 q = 0 
f3 n q = q ^ (2 * n) + q ^ ((2 * n) - 1) + f3 (n - 1) q

f3' :: Int -> Int -> Int
f3' n q = f2 (2*n) q

-- Se resta q ^ (n-1) para "borrar" los términos anteriores a n
f4 :: Int -> Int -> Int 
f4 0 q = 1
f4 n q = q ^ (2 * n) + q ^ (2 * n - 1) - q ^ (n - 1) + f4 (n - 1) q

f4' :: Int -> Int -> Int
f4' n q = f3 n q - f2 (n-1) q

factorial :: Int -> Int 
factorial 1 = 1
factorial n = n * (factorial (n - 1))

-- fromIntegral sirve para pasar un Int cuando la función ("/") exige otro tipo (Float)
eAprox :: Int -> Float 
eAprox 0 = 1
eAprox n = 1 / (fromIntegral (factorial n)) + (eAprox (n - 1))

e :: Float
e = eAprox 10

f5 :: Int -> Int -> Int
f5 0 m = 0
f5 n m = f5 (n-1) m + f2 n m
