maxi :: Int -> Int -> Int
maxi x z 
 | x >= z       = x
 | otherwise    = z

vendas :: Int -> Int
vendas 0 = 10
vendas 1 = 5
vendas 2 = 6
vendas 3 = 5
vendas _ = 10

maiorVenda :: Int -> Int
maiorVenda 0 = vendas 0
maiorVenda n = maxi(maiorVenda(n-1)) (vendas n)

zeroVendas :: Int -> Int
zeroVendas n
    | ((vendas n) == 0) = n
    | ((vendas n) /= 0) = zeroVendas(n -1)
    | otherwise     = -1

achaSemana :: Int -> Int ->Int
achaSemana s n
    | ((vendas n) == s) = n
    | ((vendas n) /= s) = achaSemana(s)(n -1)

--no sexto exercicio eu apenas colocaria s como 0 e faria retornar como -1 caso nenhuma semana

fatorial :: Int -> Int
fatorial x = x * fatorial(x-1)
fatorial 0 = 1

fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci(n-1) + fibonacci(n-2)