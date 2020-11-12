somaQuadrupla :: [(Int,Int,Int,Int)] -> Int
somaQuadrupla [] = 0
somaQuadrupla ((a,b,c,d):xs) = a + b + c + d + somaQuadrupla xs

somaTuplas :: [((Int,Int),(Int,Int))] -> Int
somaTuplas [] = 0
somaTuplas (((a,b),(c,d)):xs) = a + b + c + d + somaTuplas xs

zipp :: [Int] -> [Int] -> [(Int,Int)]
zipp x []  = []
zipp [] y  = []
zipp (x:xs) (y:ys) = (x,y) : zipp xs ys

zippTres :: [Int] -> [Int] -> [Int] -> [(Int,Int,Int)]
zippTres x [] z = []
zippTres [] y z = []
zippTres x y [] = []
zippTres (x:xs) (y:ys) (z:zs) = (x,y,z) : zippTres xs ys zs

unzipDir :: [(Int,Int)] -> [Int]
unzipDir [] = []
unzipDir ((a,b):xs) = b : unzipDir xs

unzipEsq :: [(Int,Int)] -> [Int]
unzipEsq [] = []
unzipEsq ((a,b):xs) = a : unzipEsq xs

unZipp :: [(Int,Int)] -> ([Int], [Int])
unZipp ((a,b):xs) = (unzipEsq((a,b):xs),unzipDir((a,b):xs))