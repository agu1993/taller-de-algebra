identidad x = x -- :t identidad -> muestra los tipos de variables de la función --


-- Tipos: Int, Float, Bool, etc...                                               --
-- Clases de tipos: Num, Ord, Floating, Eq, Ord, Fractional, Integral.           --
--                                                                               --
-- Cada tipo pertenece a una o más clases de tipos, que le dan características.  --
--                                                                               --
-- Tuplas:                                                                       --
-- (1, 2) :: (Int, Int)                                                          --
-- (2.2, 4.3, 5.0) :: (Float, Float, Float)                                      --
-- (True, (1, 2)) :: (Bool, (Int, Int))                                          --
-- (True, 1, 2)                                                                  --
--																				 --
-- fst :: (a, b) -> a    |	fst (1 + 4, 2)  => 5								 --
-- snd :: (a, b) -> b	 |  snd (1, (2, 3)) => (2, 3)							 --
{- investigar
sobre
módulos! -}



cinco :: Float
cinco = 5

sumaVectorial :: (Float, Float) -> (Float, Float) -> (Float, Float)
sumaVectorial v w = (fst v + fst w, snd v + snd w)

-- o también -- 

sumaVectorial2 (vx, vy) (wx, wy) = (vx + wx, vy + wy)

esOrigen :: (Float, Float) -> Bool
esOrigen (0, 0) = True
esOrigen (_, _) = False

angulo0 :: (Float, Float) -> Bool
angulo0 (_, 0) = True
angulo0 (_, _) = False

angulo45 :: (Float, Float) -> Bool
angulo45 (x, y) = x == y

normaVectorial :: (Float, Float) -> Float
normaVectorial (x, y) = sqrt (x**2 + y**2)

normaSuma :: (Float, Float) -> (Float, Float) -> Float
normaSuma v w = normaVectorial (sumaVectorial v w)


-------------------------------------- Ejercicios ----------------------------------

estanRelacionados :: Float -> Float -> Bool
estanRelacionados x y | x <= 3 && y <= 3 = True
                      | (3 < x && x <= 7) && (3 < y && y <= 7) = True
                      | x > 7 && y > 7 = True
                      | otherwise = False

prodInt :: (Float, Float) -> (Float, Float) -> Float
prodInt (vx, vy) (wx, wy) = (vx * wx) + (vy * wy)

todoMenor :: (Float, Float) -> (Float, Float) -> Bool
todoMenor (vx, vy) (wx, wy) | vx < wx && vy < wy = True
                            | otherwise = False

modulo :: Float -> Float
modulo x | x >= 0 = x
         | x < 0 = (-x)

distanciaPuntos :: (Float, Float) -> (Float, Float) -> Float
distanciaPuntos (px, py) (qx, qy) = sqrt (modulo(px - qx) ** 2 + modulo (py - qy) ** 2)

sumaTerna :: (Int, Int, Int) -> Int
sumaTerna (x, y, z) = x + y + z

posicPrimerPar :: (Int, Int, Int) -> Int
posicPrimerPar (x, y, z) | mod x 2 == 0 = 1
                         | mod y 2 == 0 = 2
                         | mod z 2 == 0 = 3
                         | otherwise = 4

crearPar :: p -> p -> (p, p)
crearPar x y = (x, y)

invertir :: (p, p) -> (p, p)
invertir (x, y) = (y, x)



