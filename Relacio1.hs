--Relacion 1--

import Test.QuickCheck

--- Ejercicio 1---

esTerna :: Integer -> Integer -> Integer -> Bool
esTerna x y z
  | (x*x) + (y*y) == (z*z) = True
  | (y*y) + (x*x) == (z*z) = True
  | otherwise = False


terna :: Integer -> Integer -> (Integer, Integer, Integer)
terna x y = ((x*x)-(y*y), (2*x*y), (x*x) + (y*y))


p_ternas x y = x>0 && y>0 && x>y ==> esTerna l1 l2 h
 where
   (l1,l2,h) = terna x y

--- Ejercicio 2---

intercambia :: (a,b) -> (b,a)
intercambia (x,y) = (y,x)


--- Ejercicio 3---


ordena2 :: Ord a => (a,a) -> (a,a)
ordena2 (x,y)
  | x > y = (y,x)
  | x <= y = (x,y)

p1_ordena2 x y = enOrden (ordena2 (x,y))
  where
    enOrden (x,y) = x<=y

p2_ordena2 x y = mismosElementos (x,y) (ordena2 (x,y))
  where
     mismosElementos (x,y) (z,v) = (x==z && y==v) || (x==v && y==z)

ordena3 :: Ord a => (a,a,a) -> (a,a,a)
ordena3 (x,y,z)
  | (x < y) && (y < z) = (x,y,z)
  | (x < z) && (z < y) = (x,z,y)
  | (y < x) && (x < z) = (y,x,z)
  | (y < z) && (z < x) = (y,z,x)
  | (z < x) && (x < y) = (z,x,y)
  | (z < y) && (y < x) = (z,y,x)

--- Ejercicio 4---
