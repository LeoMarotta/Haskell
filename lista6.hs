pegaPosicao :: Int -> [Int] ->  Int
pegaPosicao x y = y!!(x-1)

pega :: Int -> [Int] ->  [Int]
pega x y = take x y

retira :: Int -> [Int] ->  [Int]
retira x y = drop x y 

--mediaLista

pegaMaiores :: Int -> [Int] ->  [Int]
pegaMaiores x [] = []
pegaMaiores x y
    | x < head y = head y : pegaMaiores x (tail y) 
    | otherwise = pegaMaiores x (tail y)

contaMaiores :: Int -> [Int] -> Int
contaMaiores x [] = 0
contaMaiores x y
    | x < head y = 1 + contaMaiores x (tail y)
    | otherwise = contaMaiores x (tail y)

intercala :: [Int] -> [Int] -> [Int]
intercala [] []     = []
intercala (x:xs) [] = (x:xs)
intercala [] (y:ys) = (y:ys)
intercala (x:xs) (y:ys) = x : y : intercala xs ys

dupli :: [Int] -> [Int]
dupli []     = []
dupli (x:xs) = x : x : dupli xs

dropEvery :: Int -> [Char] -> [Char]
dropEvery a [] = []
dropEvery a xs = take (a-1) xs ++ dropEvery a (drop a xs)

--split


