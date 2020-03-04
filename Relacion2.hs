import Test.QuickCheck

longitudLista :: [a] -> Int
longitudLista [] = 0
longitudLista (x:xs) = 1 + length xs

listaOrdenada :: (Ord a) => [a] -> Bool
listaOrdenada [] = True
listaOrdenada [x] = True
listaOrdenada (x:y:zs) = x <= y && listaOrdenada (y:zs)

invertirLista :: [a] -> [a]
invertirLista [] = []
invertirLista (x:xs) = invertirLista xs ++ [x]

invertirListaEficaz :: [a] -> [a]
invertirListaEficaz xs = invertir xs []
  where
    invertir [] ys = ys
    invertir (x:xs) ys = invertir xs (x:ys)

square :: Integer -> Integer
square x = x * x

-- En este caso funcionmap es una funcion de orden superior
-- Tomara (a->b) "que es una funcion" como primer argumento
-- y aplicara a todos los elementos de la lista la propiedad
-- que le asignemos. En este caso square, que esta  definido
-- arriba.
funcionmap :: (a->b) -> [a] -> [b]
funcionmap f []  = []
funcionmap f (x:xs) = f x : map f xs


filtrado :: (a -> Bool) -> [a] -> [a]
filtrado p [] = []
filtrado p (x:xs)
  | p x = x : filtrado p xs
  | otherwise = filtrado p xs


--Plegado de listas--

sumarPlegado :: (Num a) => a -> a -> a
sumarPlegado x y = x + y

--- Ejercicio 1 ---

data Direction = North | South | East | West
                 deriving (Eq, Ord, Enum, Show)

(<<) :: Direction -> Direction -> Bool
(<<) direccionA direccionB = (fromEnum direccionA) < (fromEnum direccionB)


--- Ejercicio 2 ---

maximoYresto :: Ord a => [a] -> (a,[a])
maximoYresto  xs = masGrande xs []
  where
    masGrande [x] resto = (x, resto)
    masGrande (x:y:xs) resto
      | (y > x)  = masGrande (y:xs) (x:resto)
      | otherwise = masGrande (x:xs) (y:resto)


--- Ejercicio 3 ---

reparte :: [a] -> ([a],[a])
reparte [] = ([], [])
reparte [x] = ([x],[])
reparte (x:y:xs) = ( x:izq, y:der )
  where
    (izq, der) = reparte xs
