absoluto :: Int -> Int
absoluto n | n >= 0 = n
           | n < 0 = -n

maximoAbsoluto :: Int -> Int -> Int
maximoAbsoluto x y | absoluto x > absoluto  y = absoluto x
                   | otherwise = absoluto y

maximo :: Int -> Int -> Int
maximo x y | x > y = x
           | otherwise = y

maximo3 :: Int -> Int -> Int -> Int
maximo3 x y z | maximo x y > maximo y z = maximo x y
              | otherwise = maximo y z

algunoEs0 :: Float -> Float -> Bool                
algunoEs0 0 y = True
algunoEs0 x 0 = True
algunoEs0 x y = False

algunoEs02 :: Float -> Float -> Bool
algunoEs02 x y | x == 0 || y == 0 = True
               | otherwise = False

ambosSon0 :: Float -> Float -> Bool
ambosSon0 0 0 = True
ambosSon0 x y = False

esMultiploDe :: Int -> Int -> Bool
esMultiploDe x y | mod y x == 0 = True
                 | otherwise = False

digitoUnidades :: Int -> Int
digitoUnidades x = mod x 10

digitoDecenas :: Int -> Int
digitoDecenas x = digitoUnidades (div x 10)
