module Main where

import Data.Char
import System.IO
import System.Random

--------------------------------------------------------
--Tabuleiro

{-
type LBoard = [(Int,Int)]
lBoard :: LBoard
lBoard = [(0,0),(0,1),(0,2),(1,0),(1,1),(1,2),(2,0),(2,1),(2,2)]
-}

type GBoard = [[Char]]

gBoard :: GBoard
gBoard = [['-','-','-'],
          ['-','-','-'],
          ['-','-','-']]

geraLista :: Int -> a -> [a]
geraLista n x = take n (repeat x)

geraNovoTabuleiro :: Int -> GBoard
geraNovoTabuleiro n = geraLista n (geraLista n '-')

--------------------------------------------------------
--Começo do Jogo

gArr :: Int -> [t] -> t
gArr p v 
    | (((length v)-1) < p) = error "Posicao nao e valida"
    | otherwise = v!!p

uArr :: Int -> a -> [a] -> [a]
uArr p v z
    | (((length z)-1) < p) = error "Tamanho errado"
    | otherwise = take p z ++ [v] ++ drop (p+1) z

gPos :: Int -> Int -> [[a]] -> a
gPos l c z = gArr c (gArr l z)

uPos :: Int -> Int ->  a -> [[a]] -> [[a]]
uPos linha coluna valor tab
    | (linha < length tab) && (coluna < length tab) = take linha tab ++ [uArr coluna valor (gArr linha tab)] ++ drop(linha+1) tab
    | otherwise = error "Tamanho errado"

isValidPos :: Int -> Int -> Bool
isValidPos linha coluna = ((linha < 3) && (coluna < 3)) && ((linha>=0) && (coluna>=0))

testAndUp :: Int -> Int -> Char -> GBoard -> GBoard
testAndUp linha coluna valor tab
    |((gPos linha coluna tab)=='-') = uPos linha coluna valor tab
    |otherwise = error "Ocupado" 


--------------------------------------------------------
--Fim do Jogo

winO :: GBoard -> Bool
winO [[a,b,c],[d,e,f],[g,h,i]]
    |(a=='O'&&b=='O'&&c=='O')||(d=='O'&&e=='O'&&f=='O')||(g=='O'&&h=='O'&&i=='O') = True
    |(a=='O'&&d=='O'&&g=='O')||(b=='O'&&e=='O'&&h=='O')||(c=='O'&&f=='O'&&i=='O') = True
    |(a=='O'&&e=='O'&&i=='O')||(c=='O'&&e=='O'&&g=='O') = True
    |otherwise = False

winX :: GBoard -> Bool
winX [[a,b,c],[d,e,f],[g,h,i]]
    |(a=='X'&&b=='X'&&c=='X')||(d=='X'&&e=='X'&&f=='X')||(g=='X'&&h=='X'&&i=='X') = True
    |(a=='X'&&d=='X'&&g=='X')||(b=='X'&&e=='X'&&h=='X')||(c=='X'&&f=='X'&&i=='X') = True
    |(a=='X'&&e=='X'&&i=='X')||(c=='X'&&e=='X'&&g=='X') = True
    |otherwise = False

end :: GBoard -> Int
end gBoard
    |((winX gBoard)==True)=2
    |((winO gBoard)==True)=1
    |otherwise=0    

--------------------------------------------------------
-- Funções de print

printBoard :: GBoard -> String
printBoard tabu = "\n" ++ " " ++ printColu (length tabu) ++ "\n" ++ printLinha (length tabu) tabu ++ "\n"

printLinha :: Int -> GBoard -> String
printLinha 1 tabu = linha 1 tabu
printLinha t tabu = printLinha (t - 1) tabu ++ linha t tabu

printColu :: Int -> String
printColu 1 = colu 1
printColu t = printColu (t - 1) ++ colu t

linha :: Int -> GBoard -> String
linha t tabu
    |t <= 10 = " " ++ show (t-1) ++ " " ++ (addEspaco(gArr(t-1)tabu)) ++ " " ++ "\n"
    |(t > 10) && (t <= 100) = show (t - 1) ++ " " ++ (addEspaco(gArr(t-1)tabu)) ++ " " ++ "\n"

colu :: Int -> String
colu t
    |t <= 10 = "  " ++ show (t-1)
    |(t > 10) && (t <= 100) = " " ++ show (t - 1)

addEspaco :: String -> String
addEspaco [] = []
addEspaco (x:xs) = x:' ':' ':addEspaco xs

--------------------------------------------------------
-- Main

main :: IO ()
main = do
    let gb = geraNovoTabuleiro 3
    putStr "Jogo da Velha"   
    putStr "Digite uma linha jogador X: "
    linha <- getLine
    putStr "Digite uma coluna jogador X: "
    coluna <- getLine
    let newGB = testAndUp linha coluna 'X' gb
    gameLoop gb

gameLoop :: GBoard -> IO ()
gameLoop newGB = do
    putStrLn (printBoard newGB)

    if(end newGB == 0)
        then do

            putStrLn (printBoard newGB)

            putStr "Digite uma linha jogador O: "
            linha <- getLine
            putStr "Digite uma coluna jogador O: "
            coluna <- getLine
            let newGB = testAndUp linha coluna 'O' newGB

            if(end newGB == 2)
                then do
                    putStrLn "Jogador X venceu"
                else do
                    putStrLn "Jogador O venceu"

            putStrLn (printBoard newGB)
            putStr "Digite uma linha jogador X: "
            linha <- getLine
            putStr "Digite uma coluna jogador X: "
            coluna <- getLine
            let newGB = testAndUp linha coluna 'X' newGB
            gbgameLoop newGB
        else do
            if(end newGB == 2)
                then do
                    putStrLn "Jogador X venceu"
                else do
                    putStrLn "Jogador O venceu"


{-
    if(end newGB == 0)
        then do
            
        else do
            if(end gb == 2)
                then do
                    putStrLn "Jogador X venceu"
                else do
                    putStrLn "Jogador O venceu"
        

    if(end newGB == 0)
        then do
            putStrLn (printBoard gb)
            putStr "Digite uma linha jogador O: "
            linha <- getLine
            putStr "Digite uma coluna jogador O: "
            coluna <- getLine
            let newGB = testAndUp linha coluna 'O' 
            gbgameLoop newGB
        else do
            if(end newGB == 2)
                then do
                    putStrLn "Jogador X venceu"
                else do
                    putStrLn "Jogador O venceu"
-}