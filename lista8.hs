dobra :: Int -> Int
dobra x = x*x

aplicaDuasVezes :: (Int -> Int) -> Int -> Int
aplicaDuasVezes f x =  f(f x)

vendas :: Int -> Int
vendas 0 = 10
vendas 1 = 5
vendas 2 = 6
vendas 3 = 5
vendas _ = 10

vendaTotal :: (Int -> Int) -> Int -> Int
vendaTotal f 0 = f 0
vendaTotal f x = f x + vendaTotal f (x-1)

soma :: Int -> Int -> Int
soma x y = x + y

mult :: Int -> Int -> Int
mult x y = x * y

foldInt :: (Int -> Int -> Int) -> [Int] -> Int
foldInt f [] = error "erro"
foldInt f (x:[]) = x
foldInt f (x:xs) = f x (foldInt f xs)

naoEspaco :: Char -> Bool
naoEspaco x = x /= ' '

filterString :: (Char -> Bool) -> [Char] -> [Char]
filterString f []     = []
filterString f (x:xs) 
    | ((f x)==False)  = filterString f xs
    | otherwise       = x : filterString f xs

somaQuadrado :: [Int] -> Int
somaQuadrado [] = 0
somaQuadrado (x:xs) = foldInt (+) (map (dobra) (x:xs))

--iter :: Int -> (Int -> Int) -> Int -> Int
--iter a f x = 
