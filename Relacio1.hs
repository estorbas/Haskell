--Relacion 1--

import Test.QuickCheck


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


intercambia :: (a,b) -> (b,a)
intercambia (x,y) = (y,x)
