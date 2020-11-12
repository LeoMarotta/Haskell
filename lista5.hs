membro :: Int -> [Int] -> Bool
membro a [] = False
membro a (x:xs) 
    | (a==x) = True
    | (a/=x) = membro a xs

membroNum :: Int -> [Int] -> Int
membroNum a [] = 0
membroNum a (x:xs) 
    | (a==x) = 1 + membroNum a xs
    | (a/=x) = membroNum a xs

membro2 :: Int -> [Int] -> Bool
membro2 a [] = False
membro2 a (x:xs)
    |   (membroNum a (x:xs) ==0) = False
    |   (membroNum a (x:xs) > 0) = True

unico2 :: Int -> [Int] -> [Int]
unico2 a [] = []
unico2 a (x:xs)
    | a == x = unico2 a xs
    | a /= x = x:unico2 a xs

unico :: [Int] -> [Int]
unico [] = []
unico (x:xs)
    |(membroNum x xs == 0) = x:unico(xs)
    |(membroNum x xs /= 0) = unico(unico2 x xs)

maxi :: Int -> Int -> Int
maxi x z 
 | x >= z       = x
 | otherwise    = z

mini :: Int -> Int -> Int
maxi x z 
 | x <= z       = x
 | otherwise    = z

{--quickSort :: [Int] -> [Int]
quickSort [] = []
quickSort (x:xs) = quickSort (menores x xs) ++ [x] ++ quickSort (maiores x xs)

menores :: Int -> [Int] -> [Int]
menores x [] = []
menores x xs 
    | (head xs < x ) == True = xs:x 
    | (head xs < x ) == False =

maiores :: Int -> [Int] -> [Int]
maiores x xs = x []
maiores x xs 
    | (head xs > x ) == True = xs:x 
    | (head xs > x ) == False =
        --}
