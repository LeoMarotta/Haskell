multDoisLista :: [Int] -> [Int]
multDoisLista [] = []
multDoisLista (x:xs) = (2*x) : multDoisLista xs

tamanho :: [Int] -> Int
tamanho [] = 0
tamanho (x:xs) = 1 + tamanho xs

produtoLista :: [Int] -> Int
produtoLista [] = 1
produtoLista (x:xs) = x * produtoLista xs

andLista :: [Bool] -> Bool
andLista [] = True
andLista (x:xs) = x && andLista xs

concatLista :: [[Int]] ->[Int]
concatLista [] = []
concatLista (x:xs) = x ++ concatLista xs

inverteLista :: [Int] -> [Int]
inverteLista [] = []
inverteLista (x:xs) = inverteLista xs ++ [x]