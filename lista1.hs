osQuatroSaoIguais :: Int -> Int -> Int -> Int -> Bool
osQuatroSaoIguais a b c d
    | (a==b) && (b==c) && (c==d)    = True
    | otherwise                     = False

quantosSaoIguais :: Int -> Int -> Int -> Int
quantosSaoIguais a b c
    | (a==b) && (b==c) && (a==c)    = 3
    | (a==b) && (b/=c) && (a/=c)    = 2
    | (a/=b) && (b==c) && (a/=c)    = 2
    | (a/=b) && (b/=c) && (a==c)    = 2
    | (a/=b) && (b/=c) && (a/=c)    = 0

todosDiferentes :: Int -> Int -> Int -> Bool
todosDiferentes a b c
    |   (a/=b) && (b/=c) && (a/=c)    = True
    |   otherwise                     = False

--no quarto exercicio n e p ainda podem ser iguais

quantosSaoIguaisV2 :: Int -> Int -> Int -> Int
quantosSaoIguaisV2 a b c
    | ((todosDiferentes a b c)==True)   = 0
    | ((osQuatroSaoIguais a b c)==True) = 3
    | otherwise                         = 2

elevadoDois :: Int -> Int
elevadoDois x = x * x

elevadoQuatro :: Int -> Int
elevadoQuatro x = (elevadoDois x) * (elevadoDois x)

vendas :: Int -> Int
vendas 0 = 10
vendas 1 = 5
vendas 2 = 6
vendas 3 = 5
vendas _ = 10

vendaTotal :: Int -> Int
vendaTotal 0 = vendas 0
vendaTotal n = vendas n + vendaTotal(n-1)
