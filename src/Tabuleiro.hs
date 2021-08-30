module Tabuleiro where

import qualified Data.Map as Map
import Tipos
import Data.List

-- recebe uma chave que eh uma posicao e retorna o valor que eh a Casa do Tabuleiro
getCasaTabuleiro :: Posicao -> Tabuleiro -> CasaTabuleiro
getCasaTabuleiro posi tab = tab Map.! posi

getNomePecasCasaTabuleiro :: Posicao -> Tabuleiro -> String
getNomePecasCasaTabuleiro posi tab = do
  let casaTabuleiro = getCasaTabuleiro posi tab
  intercalate "" [nomePeca x | x <- casaTabuleiro]
  
-- adiciona uma casa ou modifica a casa existente num tabuleiro
adicionaCasaTabuleiro :: CasaTabuleiro -> Posicao -> Tabuleiro -> Tabuleiro
adicionaCasaTabuleiro casaTab posi = Map.insert posi casaTab

-- gera a lista com todas as posicoes do tabuleiro (gera de 1 a numLin linhas, 1 a numCol colunas e faz a concatenacao para gerar o produto cartesiano)
geraListaPosicoesTabuleiro :: Int -> Int -> [Posicao]
geraListaPosicoesTabuleiro numLin numCol =
    [(lin, col) :: Posicao | lin <- [1..numLin], col <- [1..numCol]]

-- organiza a lista de posicoes em formato de matriz
geraMatrizPosicoesTabuleiro :: Int -> Int -> [[Posicao]]
geraMatrizPosicoesTabuleiro numLin numCol =
    [take numLin (drop (x * numLin) (geraListaPosicoesTabuleiro numLin numCol)) | x <- [0..numLin-1]]

getCasasTabuleiroVoltaDuas :: [Posicao]
getCasasTabuleiroVoltaDuas = [(13,9),(9,3),(7,13),(3,7)]

getPosicaoBaseInicial :: Cor -> Posicao
getPosicaoBaseInicial c
  | c == Amarelo = (14, 6)
  | c == Vermelho = (6, 2)
  | c == Verde = (2, 10)
  | c == Azul = (10, 14)
  | otherwise = (0,0)

posicaoDeBaseInicial :: Posicao -> Bool
posicaoDeBaseInicial (lin, col)
  | (lin, col) == (6,2) || (lin, col) == (2,10) || (lin, col) == (10,14) || (lin, col) == (14,6) = True
  | otherwise = False

posicaoDeBaseFinal :: Posicao -> Bool
posicaoDeBaseFinal (lin, col)
  | (lin, col) == (8,7) || (lin, col) == (7,8) || (lin, col) == (8,9) || (lin, col) == (9,8) = True
  | otherwise = False

posicaoDeMovimentacao :: Posicao -> Bool
posicaoDeMovimentacao (lin, col)
  | (lin, col) /= (8,8) && (lin, col) /= (7,7) && (lin, col) /= (9,9) && (lin, col) /= (9,7) && (lin, col) /= (7,9) && (lin `elem` [7..9] || col `elem` [7..9]) || posicaoDeBaseInicial (lin, col) = True
  | otherwise = False

geraTabuleiroVazio :: Int -> Int -> Tabuleiro
geraTabuleiroVazio numLin numCol = Map.fromList ([(posicao, []) | posicao <- geraListaPosicoesTabuleiro numLin numCol])

-- printa as casas do tabuleiro delimitando com colchetes
printCasasTabuleiro :: [Posicao] -> Tabuleiro -> String
printCasasTabuleiro [] _ = ""
printCasasTabuleiro ((l,c):t) tab
      | posicaoDeMovimentacao (l, c) = do
      if lenNomesPeca == 0 then 
        if (l, c) `elem` getCasasTabuleiroVoltaDuas 
          then "[   -2   ]" ++ printCasasTabuleiro t tab
          else "[        ]" ++ printCasasTabuleiro t tab
        else if lenNomesPeca == 2 then "[   "++ nomesPecas ++"   ]" ++ printCasasTabuleiro t tab
        else if lenNomesPeca == 4 then "[  "++ nomesPecas ++"  ]" ++ printCasasTabuleiro t tab
        else if lenNomesPeca == 6 then "[ "++ nomesPecas ++" ]" ++ printCasasTabuleiro t tab
        else "["++ nomesPecas ++"]" ++ printCasasTabuleiro t tab
    |otherwise = "          " ++ printCasasTabuleiro t tab
    where
      nomesPecas = getNomePecasCasaTabuleiro (l, c) tab
      lenNomesPeca = length nomesPecas 

printTabuleiro :: [[Posicao]] -> Tabuleiro -> String
printTabuleiro [] _ = ""
printTabuleiro (listPosi:t) tab = printCasasTabuleiro listPosi tab ++ "\n" ++ printTabuleiro t tab
