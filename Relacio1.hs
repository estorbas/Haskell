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

max2 :: Ord a => a -> a -> a
max2 x y
  | x > y = x
  | otherwise = y

p1_max2 :: Ord a => a -> a -> Bool
p1_max2 x y
   | max2 x y == x = True
   | max2 x y == y = True
   | otherwise = False

p2_max2 :: Ord a => a -> a -> Bool
p2_max2 x y
  | max2 x y >= x = True
  | max2 x y >= y = True
  | otherwise = False

p3_max2 :: Ord a => a -> a -> Bool
p3_max2 x y
  | max2 x y == x && x >= y = True
  | otherwise = False

p4_max2 :: Ord a => a -> a -> Bool
p4_max2 x y
  | max2 x y == y && y >= x = True
  | otherwise = False

--- Ejercicio 5 ---

entre :: Ord a => a -> (a,a) -> Bool
entre x (y,z)
    | ( x >= y && x <= z ) || (x >= z && x <= y) = True
    | otherwise = False

--- Ejercicio 6 ---

iguales3 :: Eq a => (a,a,a) -> Bool
iguales3 (x,y,z)
  | (x == y) && (y == z) = True
  | (x == z) && (z == y) = True
  | (y == x) && (x == z) = True
  | (y == z) && (z == x) = True
  | (z == x) && (x == y) = True
  | (z == y) && (y == x) = True
  | otherwise = False

--- Ejercicio 7 ---

type TotalSegundos = Integer
type Horas = Integer
type Minutos = Integer
type Segundos = Integer
descomponer :: TotalSegundos -> (Horas,Minutos,Segundos)
descomponer x = (horas,minutos,segundos)
  where
      horas = div x 3600
      minutos = div (mod x 3600) 60
      segundos = mod (mod x 3000) 60

p_descomponer x = x>=0 ==> h*3600 + m*60 + s == x
                           && entre m (0,59)
                           && entre s (0,59)
                            where (h,m,s) = descomponer x

--- Ejercicio 8 ---

unEuro :: Double
unEuro = 166.386

pesetasAEuros :: Double -> Double
pesetasAEuros x =  x / unEuro

eurosAPesetas :: Double -> Double
eurosAPesetas x = x * unEuro


--- Ejercicio 10 ---

raices :: Double -> Double -> Double -> (Double, Double)
raices a b c
  | raiz > 0 = (solucion1,solucion2)
  | otherwise =  error "Raices no reales"
    where
      raiz = sqrt (-4) * a * c
      solucion1 = ((-b + raiz)/(2*a))
      solucion2 =((-b - raiz)/(2*a))
