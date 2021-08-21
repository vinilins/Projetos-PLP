{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE InstanceSigs #-}
module Tabuleiro where

import qualified Data.Map as Map
import System.IO
import System.Process


-- Tipos.hs
data Cor =  Amarelo | Vermelho | Verde | Azul deriving (Eq, Ord, Show, Read, Bounded, Enum)

data Movimento = Cima | Baixo | Esquerda | Direita | CimaEsquerda | CimaDireita | BaixoEsquerda | BaixoDireita deriving (Eq, Ord, Show, Read, Bounded, Enum)

type Linha = Int
type Coluna = Int
type Posicao = (Linha, Coluna) -- (Linha, Coluna)

data Jogador = Jogador {
    corJogador :: Cor,
    pecas :: [Peca]
} deriving (Eq, Ord, Show, Read)


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

-- (ler de trás pra frente) map.filter: trás todo mapa que contem aquela peca; map.keys: retorna uma lista com as chaves do map.filter; head: retorna apenas a cabeca daquela lista de chaves
getPosicaoPeca :: Peca -> Tabuleiro -> Posicao
getPosicaoPeca peca tab = head $ Map.keys $ Map.filter (elem peca) tab

-- recebe uma chave que eh uma posicao e retorna o valor que eh a Casa do Tabuleiro
getCasaTabuleiro :: Posicao -> Tabuleiro -> CasaTabuleiro
getCasaTabuleiro posi tab = tab Map.! posi

-- cria um novo tabuleiro com a peca a ser removida e o retorna
removePecaDePosicao :: Peca -> Posicao -> Tabuleiro -> Tabuleiro
removePecaDePosicao peca posi tab = Map.insert posi (remove peca (getCasaTabuleiro posi tab)) tab

-- cria um novo tabuleiro com a peca a ser adicionada e o retorna
adicionaPecaEmPosicao :: Peca -> Posicao -> Tabuleiro -> Tabuleiro
adicionaPecaEmPosicao peca posi tab = Map.insert posi (insert peca (getCasaTabuleiro posi tab)) tab

-- adiciona uma casa ou modifica a casa existente num tabuleiro
adicionaCasaTabuleiro :: CasaTabuleiro -> Posicao -> Tabuleiro -> Tabuleiro
adicionaCasaTabuleiro casaTab posi = Map.insert posi casaTab

-- adiciona uma peca a posicao acima (decrescendo um na linha) e remove a peca que estava na posicao anterior
movePecaCima :: Peca -> Posicao -> Tabuleiro -> Tabuleiro
movePecaCima  peca (lin, col) tab =
  adicionaPecaEmPosicao peca novaPosicao (removePecaDePosicao peca (lin, col) tab)
    where
      novaPosicao = (lin - 1, col)

-- adiciona uma peca a posicao abaixo (acrescentando um na linha) e remove a peca que estava na posicao anterior
movePecaBaixo :: Peca -> Posicao -> Tabuleiro -> Tabuleiro
movePecaBaixo  peca (lin, col) tab =
  adicionaPecaEmPosicao peca novaPosicao (removePecaDePosicao peca (lin, col) tab)
    where
      novaPosicao = (lin + 1, col)

-- adiciona uma peca a posicao a esquerda (decrescendo um na coluna) e remove a peca que estava na posicao anterior
movePecaEsquerda :: Peca -> Posicao -> Tabuleiro -> Tabuleiro
movePecaEsquerda  peca (lin, col) tab =
  adicionaPecaEmPosicao peca novaPosicao (removePecaDePosicao peca (lin, col) tab)
    where
      novaPosicao = (lin, col - 1)

-- adiciona uma peca a posicao a direita (acrescentando um na coluna) e remove a peca que estava na posicao anterior
movePecaDireita :: Peca -> Posicao -> Tabuleiro -> Tabuleiro
movePecaDireita  peca (lin, col) tab =
  adicionaPecaEmPosicao peca novaPosicao (removePecaDePosicao peca (lin, col) tab)
    where
      novaPosicao = (lin, col + 1)


-- adiciona uma peca a posicao a cima-esquerda (decrescendo um na coluna e na linha) e remove a peca que estava na posicao anterior
movePecaCimaEsquerda :: Peca -> Posicao -> Tabuleiro -> Tabuleiro
movePecaCimaEsquerda  peca (lin, col) tab =
  adicionaPecaEmPosicao peca novaPosicao (removePecaDePosicao peca (lin, col) tab)
    where
      novaPosicao = (lin - 1, col - 1)

-- adiciona uma peca a posicao a cima-direita (decrescendo um na linha e acrescentando um na linha) e remove a peca que estava na posicao anterior
movePecaCimaDireita :: Peca -> Posicao -> Tabuleiro -> Tabuleiro
movePecaCimaDireita  peca (lin, col) tab =
  adicionaPecaEmPosicao peca novaPosicao (removePecaDePosicao peca (lin, col) tab)
    where
      novaPosicao = (lin - 1, col + 1)

-- adiciona uma peca a posicao a baixo-direita (acrescentando um na coluna e na linha) e remove a peca que estava na posicao anterior
movePecaBaixoDireita :: Peca -> Posicao -> Tabuleiro -> Tabuleiro
movePecaBaixoDireita  peca (lin, col) tab =
  adicionaPecaEmPosicao peca novaPosicao (removePecaDePosicao peca (lin, col) tab)
    where
      novaPosicao = (lin + 1, col + 1)


-- adiciona uma peca a posicao a baixo-esquerda (acrescentando um na linha e decrescendo um na linha) e remove a peca que estava na posicao anterior
movePecaBaixoEsquerda :: Peca -> Posicao -> Tabuleiro -> Tabuleiro
movePecaBaixoEsquerda  peca (lin, col) tab =
  adicionaPecaEmPosicao peca novaPosicao (removePecaDePosicao peca (lin, col) tab)
    where
      novaPosicao = (lin + 1, col - 1)

-- retorna a lista de movimentos necessarios para que uma peca obtenha a vitoria
getListaMovimentosVitoria :: Cor -> [Movimento]
getListaMovimentosVitoria cor
  | cor == Amarelo = replicate 1 Direita ++ replicate 4 Cima ++ replicate 1 CimaEsquerda ++ replicate 5 Esquerda ++ replicate 2 Cima ++ replicate 5 Direita ++
   replicate 1 CimaDireita ++ replicate 5 Cima ++ replicate 2 Direita ++ replicate 5 Baixo ++ replicate 1 BaixoDireita ++ replicate 5 Direita ++
    replicate 2 Baixo ++ replicate 5 Esquerda ++ replicate 1 BaixoEsquerda ++ replicate 5 Baixo ++ replicate 1 Esquerda ++ replicate 6 Cima
  | cor == Vermelho = replicate 1 Baixo ++ replicate 4 Direita ++ replicate 1 CimaDireita ++ replicate 5 Cima ++ replicate 2 Direita ++
   replicate 5 Baixo ++ replicate 1 BaixoDireita ++ replicate 5 Direita ++ replicate 2 Baixo ++ replicate 5 Esquerda ++ replicate 1 BaixoEsquerda ++
    replicate 5 Baixo ++ replicate 2 Esquerda ++ replicate 5 Cima ++ replicate 1 CimaEsquerda ++ replicate 5 Esquerda ++ replicate 1 Cima ++
     replicate 6 Direita
  | cor == Verde = replicate 1 Esquerda ++ replicate 4 Baixo ++ replicate 1 BaixoDireita ++ replicate 5 Direita ++ replicate 2 Baixo ++
   replicate 5 Esquerda ++ replicate 1 BaixoEsquerda ++ replicate 5 Baixo ++ replicate 2 Esquerda ++ replicate 5 Cima ++
    replicate 1 CimaEsquerda ++ replicate 5 Esquerda ++ replicate 2 Cima ++ replicate 5 Direita ++ replicate 1 CimaDireita ++
     replicate 5 Cima ++ replicate 1 Direita ++ replicate 6 Baixo
  | cor == Azul = replicate 1 Cima ++ replicate 4 Esquerda ++ replicate 1 BaixoEsquerda ++ replicate 5 Baixo ++ replicate 2 Esquerda ++
   replicate 5 Cima ++ replicate 1 CimaEsquerda ++ replicate 5 Esquerda ++ replicate 2 Cima ++ replicate 5 Direita ++ replicate 1 CimaDireita ++
    replicate 5 Cima ++ replicate 2 Direita ++ replicate 5 Baixo ++ replicate 1 BaixoDireita ++ replicate 5 Direita ++ replicate 1 Baixo ++
     replicate 6 Esquerda
  | otherwise = []

-- gera a lista com todas as posicoes do tabuleiro (gera de 1 a 15 linhas, 1 a 15 colunas e faz a concatenacao para gerar o produto cartesiano)
geraListaPosicoesTabuleiro :: Int -> Int -> [Posicao]
geraListaPosicoesTabuleiro numLin numCol =
    [(lin, col) :: Posicao | lin <- [1..numLin], col <- [1..numCol]]

-- organiza a lista de posicoes em formato de matriz
geraMatrizPosicoesTabuleiro :: Int -> Int -> [[Posicao]]
geraMatrizPosicoesTabuleiro numLin numCol =
    [take numLin (drop (x * numLin) (geraListaPosicoesTabuleiro numLin numCol)) | x <- [0..numLin-1]]

-- printa as casas do tabuleiro delimitando com colchetes
printCasasTabuleiro :: [Posicao] -> Tabuleiro -> String
printCasasTabuleiro [] _ = ""
printCasasTabuleiro ((l,c):t) tab
    | posicaoDeMovimentacao (l, c) = 
      if lenCasaTabuleiro == 0 then "[   ]" ++ printCasasTabuleiro t tab
      else "[ x ]" ++ printCasasTabuleiro t tab
    |otherwise = "     " ++ printCasasTabuleiro t tab
    
    where
        lenCasaTabuleiro = length (getCasaTabuleiro (l,c) tab)

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

posicaoBase :: Posicao -> Bool
posicaoBase (lin, col)
  | (lin, col) == (6,2) || (lin, col) == (2,10) || (lin, col) == (10,14) || (lin, col) == (14,6) = True
  | otherwise = False

posicaoDeMovimentacao :: Posicao -> Bool
posicaoDeMovimentacao (lin, col)
  | (lin, col) /= (8,8) && (lin, col) /= (7,7) && (lin, col) /= (9,9) && (lin, col) /= (9,7) && (lin, col) /= (7,9) && (lin `elem` [7..9] || col `elem` [7..9]) || (lin, col) == (6,2) || (lin, col) == (2,10) || (lin, col) == (10,14) || (lin, col) == (14,6) = True
  | otherwise = False

jogaDado :: Int
jogaDado = 3

jogadorTemMovimento :: Jogador -> Bool
jogadorTemMovimento jog = True

{-
menuMovimentaPeca :: Tabuleiro -> Jogador -> Int -> IO Tabuleiro
menuMovimentaPeca tab jog dado = do
  | jogadorTemMovimento jog = 
    system "cls" -- limpa a tela (windows somente)
    putStrLn "------------------------------------------------------------------------------\n"
    putStrLn ("Dado: " ++ show(dado)) 
    putStrLn "-------------------------------- Escolha Peca --------------------------------\n"
    putStrLn (printTabuleiro (geraMatrizPosicoesTabuleiro 15 15) tab)
    putStrLn "(1) Jogar Dado"
    putStrLn "(2) Voltar"
    putStrLn "-----\nOpção: "
    op <- getChar
    getChar -- descarta o Enter
    executarOpcao tab jog1 op
  | otherwise = 
    putStrLn "Jogador não tem opções de movimentação de peças"
    executarOpcao tab jog1 op
-}

{-
executarOpcao :: Tabuleiro -> Jogador -> Char -> IO Tabuleiro
executarOpcao tab jog '1' = do
  let valorDado = jogaDado
  ludo newTab peca
executarOpcao tab peca '2' = do
  let newTab = movePecaBaixo peca (getPosicaoPeca peca tab) tab
  menu newTab peca
executarOpcao tab peca _ = do
  putStrLn ("\nOpção inválida! Tente novamente...")
  putStrLn "Pressione <Enter> para voltar ao menu..."
  getChar
  menu tab peca
-}

ludo :: Tabuleiro -> Jogador -> Jogador -> Int -> IO Tabuleiro
ludo tab jog1 jog2 vez = do
  system "cls" -- limpa a tela (windows somente)
  putStrLn "-------------------------------- Ludo Teste --------------------------------\n"
  putStrLn (printTabuleiro (geraMatrizPosicoesTabuleiro 15 15) tab)
  putStrLn "(1) Jogar Dado"
  putStrLn "(2) Desistir"
  putStrLn "-----\nOpção: "
  op <- getChar
  getChar -- descarta o Enter
  ludo tab jog1 jog2 vez
  --executarOpcao tab jog1 op

geraTabuleiroVazio :: Int -> Int -> Tabuleiro
geraTabuleiroVazio numLin numCol = Map.fromList ([(posicao, []) | posicao <- geraListaPosicoesTabuleiro numLin numCol])

main :: IO()
main = do
  let pecaAmarelo01 = Peca Amarelo "Amarelo01" (getListaMovimentosVitoria Amarelo)
  let pecaAmarelo02 = Peca Amarelo "Amarelo02" (getListaMovimentosVitoria Amarelo)
  let pecaAmarelo03 = Peca Amarelo "Amarelo03" (getListaMovimentosVitoria Amarelo)
  let pecaAmarelo04 = Peca Amarelo "Amarelo04" (getListaMovimentosVitoria Amarelo)

  let pecaVerde01 = Peca Verde "Verde01" (getListaMovimentosVitoria Verde)
  let pecaVerde02 = Peca Verde "Verde02" (getListaMovimentosVitoria Verde)
  let pecaVerde03 = Peca Verde "Verde03" (getListaMovimentosVitoria Verde)
  let pecaVerde04 = Peca Verde "Verde04" (getListaMovimentosVitoria Verde)

  let jogador1 = Jogador Amarelo [pecaAmarelo01, pecaAmarelo02, pecaAmarelo03, pecaAmarelo04]
  let jogador2 = Jogador Verde [pecaVerde01, pecaVerde02, pecaVerde03, pecaVerde04]

  let casaTabuleiroBaseJogador1 = pecas jogador1
  let posicaoBaseJogador1 =  (14, 6) :: Posicao

  let casaTabuleiroBaseJogador2 = pecas jogador2
  let posicaoBaseJogador2 =  (2, 10) :: Posicao

  let tabuleiro = adicionaCasaTabuleiro casaTabuleiroBaseJogador2 posicaoBaseJogador2 (adicionaCasaTabuleiro casaTabuleiroBaseJogador1 posicaoBaseJogador1 (geraTabuleiroVazio 15 15))

  ludo tabuleiro jogador1 jogador2 1
  return ()
