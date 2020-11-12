somaTuplas :: ((Int,Int),(Int,Int)) -> Int
somaTuplas ((a,b),(c,d)) =a+b+c+d

shift :: ((Int, Int), Int) -> (Int, (Int, Int))
shift ((a, b), c) = (c, (a, b))

maxi :: Int -> Int -> Int -> Int
maxi x y z 
 | (x >= y) && (x>=z)      = x
 | (y >= x) && (y>=z)      = y
 | otherwise    = z

mini :: Int -> Int -> Int -> Int
mini x y z 
 | (x <= y) && (x<=z)      = x
 | (y <= x) && (y<=z)      = y
 | otherwise    = z
 
minEmax :: Int -> Int -> Int -> (Int, Int)
minEmax a b c = (maxi a b c, mini a b c)

vendas :: Int -> Int
vendas 0 = 10
vendas 1 = 5
vendas 2 = 6
vendas 3 = 5
vendas _ = 10

zeroVendas :: Int -> (Int,Bool)
zeroVendas n
    | ((vendas n) == 0) = (n,True)
    | otherwise     = (-1,False)

type Livro = (String,String,Int)

titulo:: Livro -> String
titulo (a, b, c) = a

autor:: Livro -> String
autor (a, b, c) = b

isbn:: Livro -> Int
isbn (a, b, c) = c


