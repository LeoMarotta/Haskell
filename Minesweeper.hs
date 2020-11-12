module Main where

-- Aqui estou importando algumas funções para transformar de inteiros para caracteres
--  e vice-vesa, funções de entrada/saída e números aleatóreos:

import Data.Char
import System.IO
import System.Random

-- Tabuleiro do jogo:
type GBoard = [[Char]]
-- Tabuleiro que contem a posicao das minas (Mapa de Minas). True = mina, False = sem mina:
type MBoard = [[Bool]]




-- Exemplo de Tabuleiro 9x9 inicial todo fechado:
gBoard :: GBoard
gBoard = [['-','-','-','-','-','-','-','-','-'],
          ['-','-','-','-','-','-','-','-','-'],
          ['-','-','-','-','-','-','-','-','-'],
          ['-','-','-','-','-','-','-','-','-'],
          ['-','-','-','-','-','-','-','-','-'],
          ['-','-','-','-','-','-','-','-','-'],
          ['-','-','-','-','-','-','-','-','-'],
          ['-','-','-','-','-','-','-','-','-'],
          ['-','-','-','-','-','-','-','-','-']]

-- Exemplo de tabuleiro 9x9 com a posição das minas:

mBoard :: MBoard
mBoard = [[False, False, False, False, False, False, False, False, False],
          [False, False, False, False, False, False, False, False, False],
          [False, False, False, False, False, False, False, False, False],
          [False, False, False, False, False, False, False, False, False],
          [False, False, False, False, True , False, False, False, False],
          [False, False, False, False, False, True, False, False, False],
          [False, False, False, False, False, False, False, False, False],
          [False, False, False, False, False, False, False, False, False],
          [False, False, False, False, False, False, False, False, False]]




-- PRIMEIRA PARTE - FUNÇÕES PARA MANIPULAR OS TABULEIROS DO JOGO (MATRIZES)

-- A ideia das próximas funções é permitir que a gente acesse uma lista usando um indice,
-- como se fosse um vetor

-- gArr (get array): recebe uma posicao (p) e uma lista (vetor) e devolve o elemento
-- na posição p do vetor

tamanho :: [t] -> Int
tamanho [] = 0
tamanho (x:xs) = 1 + tamanho xs

gArr :: Int -> [t] -> t
gArr p v 
    | (((tamanho v)-1) < p) = error "Posicao nao e valida"
    | otherwise = v!!p

-- uArr (update array): recebe uma posição (p), um novo valor (v), e uma lista (vetor) e devolve um
-- novo vetor com o valor v na posição p 

uArr :: Int -> a -> [a] -> [a]
uArr p v z
    | (((tamanho z)-1) < p) = error "Tamanho errado"
    | otherwise = take p z ++ [v] ++ drop (p+1) z

-- Uma matriz, nada mais é do que um vetor de vetores. 
-- Dessa forma, usando as operações anteriores, podemos criar funções para acessar os tabuleiros, como 
-- se  fossem matrizes:

-- gPos (get position) recebe linha (l), coluna (c) (não precisa validar) e um tabuleiro. Devolve o elemento na posicao
-- tabuleiro[l,c]. Usar gArr na implementação

gPos :: Int -> Int -> [[a]] -> a
gPos l c z = gArr c (gArr l z)

-- uPos (update position): recebe um novo valor, uma posição no tabuleiro (linha e coluna) e um tabuleiro. Devolve 
-- o tabuleiro modificado com o novo valor na posiçao lxc

uPos :: Int -> Int ->  a -> [[a]] -> [[a]]
uPos linha coluna valor tab
    | (linha < length tab) && (coluna < tamanho tab) = take linha tab ++ [uArr coluna valor (gArr linha tab)] ++ drop(linha+1) tab
    | otherwise = error "Tamanho errado"

--------------- SEGUNDA PARTE: LÓGICA DO JOGO

-- isMine: recebe linha coluna e o tabuleiro de minas, e diz se a posição contém uma mina

isMine :: Int -> Int -> MBoard -> Bool
isMine linha coluna tabu = gPos linha coluna tabu


-- isValidPos: recebe o tamanho do tabuleiro (ex, em um tabuleiro 9x9, o tamanho é 9), 
-- uma linha e uma coluna, e diz se essa posição é válida no tabuleiro

isValidPos :: Int -> Int -> Int -> Bool
isValidPos t linha coluna = ((linha < t) && (coluna < t)) && ((linha>=0) && (coluna>=0))

-- validMoves: Dado o tamanho do tabuleiro e uma posição atual (linha e coluna), retorna uma lista
-- com todas as posições adjacentes à posição atual

-- Exemplo: Dada a posição linha 3, coluna 3, as posições adjacentes são: [(2,2),(2,3),(2,4),(3,2),(3,4),(4,2),(4,3),(4,4)]
-- ...   ...      ...    ...   ...
-- ...  (2,2)    (2,3)  (2,4)  ...
-- ...  (3,2)    (3,3)  (3,4)  ...
-- ...  (4,2)    (4,3)  (4,4)  ...
-- ...   ...      ...    ...   ...

--  Dada a posição (0,0) que é um canto, as posições adjacentes são: [(0,1),(1,0),(1,1)]

--  (0,0)  (0,1) ...
--  (1,0)  (1,1) ...
--   ...    ...  ..

createPos :: Int -> Int -> [(Int,Int)]
createPos linha coluna = [(linha-1,coluna-1),(linha-1,coluna),(linha-1,coluna+1),(linha,coluna-1),(linha,coluna+1),(linha+1,coluna-1),(linha+1,coluna),(linha+1,coluna+1)]

filtra :: Int -> Int -> Int -> [(Int,Int)] -> [(Int,Int)]
filtra _ _ _ [] = []
filtra t x y ((linha,coluna):xys)
    |not (isValidPos t linha coluna) = filtra t x y xys
    |otherwise = (linha,coluna) : filtra t x y xys

validMoves :: Int -> Int -> Int -> [(Int,Int)]
validMoves t linha coluna
    | (isValidPos t linha coluna == False) = error "erro"
    | otherwise = filtra t linha coluna (createPos linha coluna)

-- cMinas: recebe uma posicao  (linha e coluna), o tabuleiro com o mapa das minas, e conta quantas minas
-- existem nas posições adjacentes

contaM :: [(Int,Int)] -> MBoard -> Int
contaM [] x = 0
contaM ((linha,coluna):resto) x 
    | (isMine linha coluna x) == False = contaM resto x
    | (isMine linha coluna x) == True  = 1 + contaM resto x

cMinas :: Int -> Int -> MBoard -> Int
cMinas linha coluna mBoard = contaM (validMoves (length mBoard) linha coluna) mBoard


--- abreJogada: é a função principal do jogo!!
--- recebe uma posição a ser aberta (linha e coluna), o mapa de minas e o tabuleiro do jogo. Devolve como
--  resposta o tabuleiro do jogo modificado com essa jogada.
--- Essa função é recursiva, pois no caso da entrada ser uma posição sem minas adjacentes, o algoritmo deve
--- seguir abrindo todas as posições adjacentes até que se encontre posições adjacentes à minas.
--- Vamos analisar os casos:
--- - Se a posição a ser aberta é uma mina, o tabuleiro não é modificado e encerra
--- - Se a posição a ser aberta já foi aberta, o tabuleiro não é modificado e encerra
--- - Se a posição a ser aberta é adjacente a uma ou mais minas, devolver o tabuleiro modificado com o número de
--- minas adjacentes na posição aberta
--- - Se a posição a ser aberta não possui minas adjacentes, abrimos ela com zero (0) e recursivamente abrimos
--- as outras posições adjacentes a ela

abreJogada :: Int -> Int -> MBoard -> GBoard -> GBoard
abreJogada linha coluna minas tabu 
    | ( isMine linha coluna minas ) == True = tabu
    | ( ( gPos linha coluna tabu ) /= '-' ) = tabu
    | ( (isMine linha coluna minas) == False ) && ((cMinas linha coluna minas) /= 0) = uPos linha coluna (intToDigit(cMinas linha coluna minas)) tabu
    | ( (isMine linha coluna minas) == False ) && ((cMinas linha coluna minas) == 0) = abreJogada2 (validMoves (length minas) linha coluna) minas (uPos linha coluna '0' tabu)

abreJogada2 :: [(Int,Int)] -> MBoard -> GBoard -> GBoard
abreJogada2 [] minas tabu = tabu 
abreJogada2 ((linha,coluna):resto) minas tabu
    | ( isMine linha coluna minas ) == True = tabu
    | otherwise = abreJogada2 resto minas (uPos linha coluna (intToDigit(cMinas linha coluna minas)) tabu)


--- abreTabuleiro: recebe o mapa de Minas e o tabuleiro do jogo, e abre todo o tabuleiro do jogo, mostrando
--- onde estão as minas e os números nas posições adjecentes às minas. Essa função é usada para mostrar
--- todo o tabuleiro no caso de vitória ou derrota

abreTabuleiro :: MBoard -> GBoard -> GBoard
abreTabuleiro minas tabu = funcao3 (funcao (length minas) (length minas)) minas tabu

funcao :: Int -> Int -> [(Int,Int)]
funcao t 0 = []
funcao t x = funcao2 t (geraLista t (x-1)) ++ funcao t (x-1)

funcao2 :: Int -> [Int] -> [(Int,Int)]
funcao2 t x =  zip x ( take t ( iterate ( \x -> x+1 ) 0 ) )

funcao3 :: [(Int,Int)] -> MBoard -> GBoard -> GBoard
funcao3 [] minas tabu = tabu
funcao3 ((linha,coluna):xys) minas tabu
    | isMine linha coluna minas == True = funcao3 xys minas (uPos linha coluna '*' tabu)
    | otherwise = funcao3 xys minas (uPos linha coluna (intToDigit (cMinas linha coluna minas)) tabu)

--  -- contaFechadas: Recebe um GBoard e conta quantas posições fechadas existem no tabuleiro (posições com '-')

contaFechadas :: GBoard -> Int
contaFechadas [] = 0
contaFechadas (x:xs) = contador x + contaFechadas xs

contador :: [Char] -> Int
contador [] = 0
contador (x:xs) 
    | x == '-'  = 1 + contador xs 
    | otherwise = contador xs

-- contaMinas: Recebe o tabuleiro de Minas (MBoard) e conta quantas minas existem no jogo

contaMinas :: MBoard -> Int
contaMinas [] = 0
contaMinas (x:xs) = contadorM x + contaMinas xs

contadorM :: [Bool] -> Int
contadorM [] = 0
contadorM (x:xs)
    | x == True = 1 + contadorM xs
    | otherwise = contadorM xs

-- endGame: recebe o tabuleiro de minas, o tauleiro do jogo, e diz se o jogo acabou.
-- O jogo acabou quando o número de casas fechadas é igual ao numero de minas

endGame :: MBoard -> GBoard -> Bool
endGame x y
    | (contaMinas x == contaFechadas y) = True 
    | otherwise = False

---
---  PARTE 3: FUNÇÕES PARA GERAR TABULEIROS E IMPRIMIR TABULEIROS
---

-- printBoard: Recebe o tabuleiro do jogo e devolve uma string que é a representação visual desse tabuleiro
-- Usar como referncia de implementacao o video sobre tabela de vendas (Aula 06)

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

espaco :: String -> String
espaco [] = []
espaco (x:xs) = x:' ':' ':espaco xs

-- geraLista: recebe um inteiro n, um valor v, e gera uma lista contendo n vezes o valor v

geraLista :: Int -> a -> [a]
geraLista n x = take n (repeat x)

-- geraTabuleiro: recebe o tamanho do tabuleiro e gera um tabuleiro  novo, todo fechado (todas as posições
-- contém '-'). A função geraLista deve ser usada na implementação

geraNovoTabuleiro :: Int -> GBoard
geraNovoTabuleiro n = geraLista n (geraLista n '-')

-- geraMapaDeMinasZerado: recebe o tamanho do tabuleiro e gera um mapa de minas zerado, com todas as posições
-- contendo False. Usar geraLista na implementação

geraMapaDeMinasZerado :: Int -> MBoard
geraMapaDeMinasZerado n = geraLista n (geraLista n False)


-- A função a seguir (main) deve ser substituida pela função main comentada mais
-- abaixo quando o jogo estiver pronto

main :: IO ()
main = print "Alo Mundo!"

{-

-- Aqui está o Motor do Jogo.
-- Essa parte deve ser descomentada quando as outras funções estiverem implementadas
-- Para rodar o jogo, digite "main" no interpretador

main :: IO ()
main = do
   putStr "Digite o tamanho do tabuleiro: "
   size <- getLine
   mb <- genMinesBoard (read size)
   gameLoop mb (geraNovoTabuleiro (read size)) 

gameLoop :: MBoard -> GBoard -> IO ()
gameLoop mb gb = do
   putStr (printBoard gb)
   putStr "Digite uma linha: "
   linha <- getLine
   putStr "Digite uma coluna: "
   coluna <- getLine
   if (isMine (read linha) (read coluna) mb)
      then do
            putStr "VOCE PERDEU!\n"
            putStr $ printBoard $ abreTabuleiro mb gb
            putStr "TENTE NOVAMENTE!\n"
      else do
            let newGB = (abreJogada (read linha) (read coluna) mb gb)
            if (endGame mb newGB)
                 then do
                     putStr "VOCE VENCEU!!!!!!!!\n"
                     putStr $ printBoard $ abreTabuleiro mb newGB
                     putStr "PARABENS!!!!!!!!!!!\n"
                 else
                     gameLoop mb newGB




----- DO NOT GO BEYOUND THIS POINT   


genMinesBoard :: Int -> IO MBoard
genMinesBoard size = do
        board <- addMines (round   ((fromIntegral (size *size)) * 0.15)) size (geraMapaDeMinasZerado size) 
        return board

addMines :: Int -> Int -> MBoard -> IO MBoard
addMines 0 size b = return b
addMines n size b = do
                l <- randomRIO (0,(size-1))
                c <- randomRIO (0,(size-1))
                case isMine l c b of
                      True -> addMines n size b
                      False -> addMines (n-1) size (uPos l c True b)

-}