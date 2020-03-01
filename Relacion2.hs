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



data Direction = North | South | East | West
                 deriving (Eq, Ord, Enum, Show)

--(<<) :: Direction -> Direction -> Bool
