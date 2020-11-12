data Arvore a = Folha a | Nodo a (Arvore a) (Arvore a)
    deriving(Eq,Show)

multDois :: Arvore Int -> Arvore Int
multDois (Folha n) = Folha (n*2)
multDois (Nodo n a1 a2) = (Nodo (n*2) (multDois a1) (multDois a2))

contaElementos :: Arvore a -> Int
contaElementos (Folha n) = 1
contaElementos (Nodo n a1 a2) = 1 + contaElementos a1 + contaElementos a2

    {--
contaLadoE :: Arvore a -> Int
contaLadoE (Folha n) = 1
contaLadoE (Nodo n a1 a2) = 1 + contaElementos a1

contaLadoD :: Arvore a -> Int
contaLadoE (Folha n) = 1
contaLadoE (Nodo n a1 a2) = 1 + contaElementos a1

altura :: Arvore a -> Int
altura (Folha n) = 1
altura (Nodo n a1 a2)
    | contaLadoE (Nodo n a1 a2) > contaLadoD (Nodo n a1 a2)
    |
    --}

quickSort :: [Int] -> [Int]
quickSort [] = []
quickSort (x:xs) =  quickSort (menores x xs) ++ [x] ++ quickSort (maiores x xs)

menores :: Int -> [Int] -> [Int]
menores n [] = []
menores n (x:xs) 
    | x <= n = x: menores n xs
    | otherwise = menores n xs

maiores :: Int -> [Int] -> [Int]
maiores n [] = []
maiores n (x:xs) 
    | x > n = x : maiores n xs
    | otherwise = maiores n xs

maiorElemento :: Arvore Int -> Int
maiorElemento x = head (drop ((length (arvoreToLista x))-1) (quickSort (arvoreToLista x)))


procuraInt :: Int -> Arvore Int -> Bool
procuraInt x (Folha n) = (n==x)
procuraInt x (Nodo n a1 a2)
    | (n==x) = True
    | otherwise = procuraInt x a1 || procuraInt x a2

quantasVezes :: Int -> Arvore Int -> Int
quantasVezes x (Folha n)
    |(n==x) = 1
    |otherwise = 0
quantasVezes x (Nodo n a1 a2)
    | (n==x) = 1 + (quantasVezes (x) (a1)) + (quantasVezes (x) (a2))
    | otherwise = (quantasVezes (x) (a1)) + (quantasVezes (x) (a2))

refleteArvore :: Arvore a -> Arvore a
refleteArvore (Folha n) = Folha n
refleteArvore (Nodo n a1 a2) = (Nodo n (refleteArvore a2) (refleteArvore a1))

arvoreToLista :: Arvore a -> [a]
arvoreToLista (Folha n) = [n]
arvoreToLista (Nodo n a1 a2) = [n] ++ (arvoreToLista (a1)) ++ (arvoreToLista (a2))

mapTree :: (a-> b) -> Arvore a -> Arvore b
mapTree x (Folha n) = (Folha (x n))
mapTree x (Nodo n a1 a2) = (Nodo (x n) (mapTree (x) (a1)) (mapTree (x) (a2)))