head2 ::[a] -> a
head2 [] = error "erro" 
head2 (a:x) = a

tail2 :: [a] -> [a]
tail2 [] = error "erro"
tail2 (a:x) = x

fst2 :: (a, b) -> a 
fst2 (t,u) = t

shift :: ((a,b), c) -> (a,(b,c))
shift ((a,b),c) = (a,(b,c))

concatena :: [[a]] -> [a]
concatena [] = []
concatena (x:xs) = x ++ concatena xs

inverte :: [a] -> [a]
inverte [] = []
inverte (x:xs) = inverte xs ++ [x]

zipp3 :: [a] -> [b] -> [c] -> [(a,b,c)]
zipp3 [] x y = []
zipp3 x [] y = []
zipp3 x y [] = []
zipp3 (x:xs) (y:ys) (z:zs) = (x,y,z) : zipp3 xs ys zs

mapMaisUm :: (Int -> a) -> [Int] -> [a] --ou float
mapMaisUm f [] = []
mapMaisUm f (x:xs) = f (x+1) : mapMaisUm f xs

--foldr :: (a -> b -> b) -> b -> [a] -> b
--foldr f v [] = v
--foldr f v (x:xs) = f x (foldr f v xs)