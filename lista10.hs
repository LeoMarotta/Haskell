concatena :: [[a]] -> [a]
concatena x = foldr(++) [] x

andLista :: [Bool] -> Bool
andLista x = foldr (&&) True x

somaQuadPos :: [Int] -> Int
somaQuadPos x = foldr (+) 0 (map (^2) (filter (>0) x))

somaListas :: [[Int]] -> Int
somaListas x =  foldr (+) 0 (foldr (++) [] x)

tamanhoListas :: [[a]] -> Int
tamanhoListas x = length(concatena x)

--inverte :: [a] -> [a]
--inverte x = foldr (++drop ((length x)-1) x) [] x

separaPalavras :: String -> [String]
separaPalavras [] = 0
separaPalavras x = (takeWhile (/= ' ') x) : (separaPalavras ( drop 1 (dropWhile (/= ' ') x)))