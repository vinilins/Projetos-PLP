{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE InstanceSigs #-}
module Tabuleiro where

import qualified Data.Map as Map


-- Tipos.hs
data Cor =  Amarelo | Vermelho | Verde | Azul deriving (Eq, Ord, Show, Read, Bounded, Enum)

data Movimento = Cima | Baixo | Esquerda | Direita | CimaEsquerda | CimaDireita | BaixoEsquerda | BaixoDireita deriving (Eq, Ord, Show, Read, Bounded, Enum)

type Linha = Int
type Coluna = Int
type Posicao = (Linha, Coluna) -- (Linha, Coluna)

data Peca = Peca {
    cor :: Cor,
    nome :: String,
    listaMovimentosVitoria :: [Movimento]
} deriving (Ord, Show, Read)

-- Definindo condição de igualdedade e diferença entre duas pecas
instance Eq Peca  where
   (==), (/=) :: Peca -> Peca -> Bool
   x /= y     =  nome x /= nome y
   x == y     =  nome x == nome y


type CasaTabuleiro = [Peca]

type Tabuleiro = Map.Map Posicao CasaTabuleiro  -- map com 225 casaTabuleiro


-- remove um elemento de uma lista
remove :: Eq a => a -> [a] -> [a]
remove element = filter (/= element)

-- adiciona um elemento em uma lista
insert :: a -> [a] -> [a]
insert element list = element : list

getPosicaoPeca :: Peca -> Tabuleiro -> Posicao
getPosicaoPeca peca tab = head $ Map.keys $ Map.filter (elem peca) tab

getCasaTabuleiro :: Posicao -> Tabuleiro -> CasaTabuleiro
getCasaTabuleiro posi tab = tab Map.! posi

removePecaDePosicao :: Peca -> Posicao -> Tabuleiro -> Tabuleiro
removePecaDePosicao peca posi tab = Map.insert posi (remove peca (getCasaTabuleiro posi tab)) tab

adicionaPecaEmPosicao :: Peca -> Posicao -> Tabuleiro -> Tabuleiro
adicionaPecaEmPosicao peca posi tab = Map.insert posi (insert peca (getCasaTabuleiro posi tab)) tab

movePecaCima :: Peca -> Posicao -> Tabuleiro -> Tabuleiro
movePecaCima  peca (lin, col) tab =
  adicionaPecaEmPosicao peca novaPosicao (removePecaDePosicao peca (lin, col) tab)
    where
      novaPosicao = (lin - 1, col)

movePecaBaixo :: Peca -> Posicao -> Tabuleiro -> Tabuleiro
movePecaBaixo  peca (lin, col) tab =
  adicionaPecaEmPosicao peca novaPosicao (removePecaDePosicao peca (lin, col) tab)
    where
      novaPosicao = (lin + 1, col)

movePecaEsquerda :: Peca -> Posicao -> Tabuleiro -> Tabuleiro
movePecaEsquerda  peca (lin, col) tab =
  adicionaPecaEmPosicao peca novaPosicao (removePecaDePosicao peca (lin, col) tab)
    where
      novaPosicao = (lin, col - 1)

movePecaDireita :: Peca -> Posicao -> Tabuleiro -> Tabuleiro
movePecaDireita  peca (lin, col) tab =
  adicionaPecaEmPosicao peca novaPosicao (removePecaDePosicao peca (lin, col) tab)
    where
      novaPosicao = (lin, col + 1)

movePecaCimaEsquerda :: Peca -> Posicao -> Tabuleiro -> Tabuleiro
movePecaCimaEsquerda  peca (lin, col) tab =
  adicionaPecaEmPosicao peca novaPosicao (removePecaDePosicao peca (lin, col) tab)
    where
      novaPosicao = (lin - 1, col - 1)

movePecaCimaDireita :: Peca -> Posicao -> Tabuleiro -> Tabuleiro
movePecaCimaDireita  peca (lin, col) tab =
  adicionaPecaEmPosicao peca novaPosicao (removePecaDePosicao peca (lin, col) tab)
    where
      novaPosicao = (lin - 1, col + 1)

movePecaBaixoDireita :: Peca -> Posicao -> Tabuleiro -> Tabuleiro
movePecaBaixoDireita  peca (lin, col) tab =
  adicionaPecaEmPosicao peca novaPosicao (removePecaDePosicao peca (lin, col) tab)
    where
      novaPosicao = (lin + 1, col + 1)

movePecaBaixoEsquerda :: Peca -> Posicao -> Tabuleiro -> Tabuleiro
movePecaBaixoEsquerda  peca (lin, col) tab =
  adicionaPecaEmPosicao peca novaPosicao (removePecaDePosicao peca (lin, col) tab)
    where
      novaPosicao = (lin + 1, col - 1)

getListaMovimentosVitoria :: Cor -> [Movimento]
getListaMovimentosVitoria cor
  | cor == Amarelo = replicate 1 Direita ++ replicate 4 Cima
  | cor == Vermelho = []
  | cor == Verde = []
  | cor == Azul = []
  | otherwise = []

geraListaPosicoesTabuleiro :: Int -> Int -> [Posicao]
geraListaPosicoesTabuleiro numLin numCol =
    [(lin, col) :: Posicao | lin <- [1..numLin], col <- [1..numCol]]

geraMatrizPosicoesTabuleiro :: Int -> Int -> [[Posicao]]
geraMatrizPosicoesTabuleiro numLin numCol =
    [take numLin (drop (x * numLin) (geraListaPosicoesTabuleiro numLin numCol)) | x <- [0..numLin-1]]

printCasasTabuleiro :: [Posicao] -> Tabuleiro -> String
printCasasTabuleiro [] _ = ""
printCasasTabuleiro ((l,c):t) tab
    | lenCasaTabuleiro == 0 =   "[   ]" ++ printCasasTabuleiro t tab
    | otherwise = "[ x ]" ++ printCasasTabuleiro t tab
    where
        lenCasaTabuleiro = length (tab Map.! (l,c))

printTabuleiro :: [[Posicao]] -> Tabuleiro -> String
printTabuleiro [] _ = ""
printTabuleiro (listPosi:t) tab = printCasasTabuleiro listPosi tab ++ "\n" ++ printTabuleiro t tab

executaMovimentoPeca :: Peca -> Posicao -> Movimento -> Tabuleiro -> Tabuleiro
executaMovimentoPeca peca posi movi tab
  | movi == Cima = movePecaCima peca posi tab
  | movi == Baixo = movePecaBaixo peca posi tab
  | movi == Esquerda = movePecaEsquerda peca posi tab
  | movi == Direita = movePecaDireita peca posi tab
  | movi == CimaEsquerda = movePecaCimaEsquerda peca posi tab
  | movi == CimaDireita = movePecaCimaDireita peca posi tab
  | movi == BaixoEsquerda = movePecaBaixoEsquerda peca posi tab
  | movi == BaixoDireita = movePecaBaixoDireita peca posi tab
  | otherwise = tab

movimentaPeca :: Peca -> Posicao -> Tabuleiro -> Tabuleiro
movimentaPeca peca posi = executaMovimentoPeca (Peca (cor peca) (nome peca) (drop 1 (listaMovimentosVitoria peca))) posi (head (listaMovimentosVitoria  peca))

main :: IO ()
main = do
    let numLinhas = 15
    let numColunas = 15
    let listaPosicoesTabuleiro = geraListaPosicoesTabuleiro numLinhas numColunas
    let matrizPosicoesTabuleiro = geraMatrizPosicoesTabuleiro numLinhas numColunas

    let casaTabuleiroNormal = [] :: CasaTabuleiro
    let tabuleiro = Map.fromList ([(posicao, casaTabuleiroNormal) | posicao <- listaPosicoesTabuleiro]) :: Tabuleiro

    let pecaAmarelo01 = Peca Amarelo "Amarelo01" (getListaMovimentosVitoria Amarelo)
    let pecaAmarelo02 = Peca Amarelo "Amarelo02" (getListaMovimentosVitoria Amarelo)
    let pecaAmarelo03 = Peca Amarelo "Amarelo02" (drop 1 (getListaMovimentosVitoria Amarelo))
    let casaTabuleiroTest1 = [pecaAmarelo01] :: CasaTabuleiro
    let casaTabuleiroTest2 = [pecaAmarelo02] :: CasaTabuleiro

    let tabuleiro2 = Map.insert (4, 8) casaTabuleiroTest2 (Map.insert (12, 8) casaTabuleiroTest1 tabuleiro) -- inicial

    let tabuleiroMovePecaCima = movePecaCima pecaAmarelo01 (12, 8) tabuleiro2
    let tabuleiroMovePecaBaixo = movePecaBaixo pecaAmarelo01 (12, 8) tabuleiro2
    let tabuleiroMovePecaEsquerda = movePecaEsquerda pecaAmarelo01 (12, 8) tabuleiro2
    let tabuleiroMovePecaDireita = movePecaDireita pecaAmarelo01 (12, 8) tabuleiro2
    let tabuleiroMovePecaCimaEsquerda = movePecaCimaEsquerda pecaAmarelo01 (12, 8) tabuleiro2
    let tabuleiroMovePecaCimaDireita = movePecaCimaDireita pecaAmarelo01 (12, 8) tabuleiro2
    let tabuleiroMovePecaBaixoDireita = movePecaBaixoDireita pecaAmarelo01 (12, 8) tabuleiro2
    let tabuleiroMovePecaBaixoEsquerda = movePecaBaixoEsquerda pecaAmarelo01 (12, 8) tabuleiro2
    let tabuleiroTestMovimentaPeca1 = movimentaPeca pecaAmarelo02 (4, 8) tabuleiro2
    let tabuleiroTestMovimentaPeca2 = movimentaPeca pecaAmarelo03 (4, 9) tabuleiroTestMovimentaPeca1

    putStr "\nPosição inicial\n"
    putStr (printTabuleiro [[(11,7),(11,8),(11,9)],[(12,7),(12,8),(12,9)],[(13,7),(13,8),(13,9)]] tabuleiro2)
    putStr "\n"

    putStr "\nmovePecaCima\n"
    putStr (printTabuleiro [[(11,7),(11,8),(11,9)],[(12,7),(12,8),(12,9)],[(13,7),(13,8),(13,9)]] tabuleiroMovePecaCima)
    putStr "\n"

    putStr "\nmovePecaBaixo\n"
    putStr (printTabuleiro [[(11,7),(11,8),(11,9)],[(12,7),(12,8),(12,9)],[(13,7),(13,8),(13,9)]] tabuleiroMovePecaBaixo)
    putStr "\n"

    putStr "\nMovePecaEsquerda\n"
    putStr (printTabuleiro [[(11,7),(11,8),(11,9)],[(12,7),(12,8),(12,9)],[(13,7),(13,8),(13,9)]] tabuleiroMovePecaEsquerda)
    putStr "\n"

    putStr "\nMovePecaDireita\n"
    putStr (printTabuleiro [[(11,7),(11,8),(11,9)],[(12,7),(12,8),(12,9)],[(13,7),(13,8),(13,9)]] tabuleiroMovePecaDireita)
    putStr "\n"

    putStr "\nMovePecaCimaEsquerda\n"
    putStr (printTabuleiro [[(11,7),(11,8),(11,9)],[(12,7),(12,8),(12,9)],[(13,7),(13,8),(13,9)]] tabuleiroMovePecaCimaEsquerda)
    putStr "\n"

    putStr "\nMovePecaCimaDireita\n"
    putStr (printTabuleiro [[(11,7),(11,8),(11,9)],[(12,7),(12,8),(12,9)],[(13,7),(13,8),(13,9)]] tabuleiroMovePecaCimaDireita)
    putStr "\n"

    putStr "\nMovePecaBaixoDireita\n"
    putStr (printTabuleiro [[(11,7),(11,8),(11,9)],[(12,7),(12,8),(12,9)],[(13,7),(13,8),(13,9)]] tabuleiroMovePecaBaixoDireita)
    putStr "\n"

    putStr "\nMovePecaBaixoEsquerda\n"
    putStr (printTabuleiro [[(11,7),(11,8),(11,9)],[(12,7),(12,8),(12,9)],[(13,7),(13,8),(13,9)]] tabuleiroMovePecaBaixoEsquerda)
    putStr "\n"

    putStr "\nTabuleiro\n"
    putStr (printTabuleiro matrizPosicoesTabuleiro tabuleiro)
    putStr "\n"

    print(tabuleiro2 Map.! (4, 8))
    print(tabuleiro2 Map.! (4, 9))
    print(tabuleiro2 Map.! (3, 9))
    putStr "\n"
    print(tabuleiroTestMovimentaPeca1 Map.! (4, 8))
    print(tabuleiroTestMovimentaPeca1 Map.! (4, 9))
    print(tabuleiroTestMovimentaPeca1 Map.! (3, 9))
    putStr "\n"
    print(tabuleiroTestMovimentaPeca2 Map.! (4, 8))
    print(tabuleiroTestMovimentaPeca2 Map.! (4, 9))
    print(tabuleiroTestMovimentaPeca2 Map.! (3, 9))
    putStr "\n"

    --print( getPecasCasaTabuleiro tabuleiro (14, 6))

    --putStrLn("\n\n")
    --print tabuleiro2
    --print (tabuleiro2 ! (15, 15))
    --print (tabuleiro2 ! (12, 7))
    --putStrLn("\n\n")


    --let map2 = insert (15, 15) 2 map 
    --print(map2) 
    --print (listaPosicoes)

    -- código para o dado
    -- > import System.Random
    -- > randomRIO (1, 6)
    -- saídas possíveis: 1,2,3,4,5,6
