module Tabuleiro where

import Data.List
import qualified Data.Map as Map

import Tipos
import Util


getNumLinhasTabuleiro :: Int
getNumLinhasTabuleiro = 15

getNumColunasTabuleiro :: Int
getNumColunasTabuleiro = 15

-- recebe uma chave que eh uma posicao e retorna o valor que eh a Casa do Tabuleiro
getCasaTabuleiro :: Posicao -> Tabuleiro -> CasaTabuleiro
getCasaTabuleiro posi tab = tab Map.! posi

-- retorna uma string comtendo o nome de todas as peças que estão na casa do tabuleiro
getNomesPecasCasaTabuleiro :: Posicao -> Tabuleiro -> String
getNomesPecasCasaTabuleiro posi tab = intercalate "" [toStringPecaComCor peca | peca <- getCasaTabuleiro posi tab]

getNumPecasCasaTabuleiro :: Posicao -> Tabuleiro -> Int
getNumPecasCasaTabuleiro posi tab = length (getCasaTabuleiro posi tab)

-- adiciona uma casa ou modifica a casa existente num tabuleiro
adicionaCasaTabuleiro :: CasaTabuleiro -> Posicao -> Tabuleiro -> Tabuleiro
adicionaCasaTabuleiro casaTab posi = Map.insert posi casaTab

-- gera a lista com todas as posicoes do tabuleiro (gera de 1 a getNumLinhasTabuleiro, e 1 a getNumColunasTabuleiroe) e faz a concatenacao para gerar o produto cartesiano)
geraListaPosicoesTabuleiro :: [Posicao]
geraListaPosicoesTabuleiro = [(lin, col) | lin <- [1..getNumLinhasTabuleiro], col <- [1..getNumColunasTabuleiro]]

-- organiza a lista de posicoes em formato de matriz
geraMatrizPosicoesTabuleiro :: [[Posicao]]
geraMatrizPosicoesTabuleiro = [take getNumLinhasTabuleiro (drop (numLin * getNumLinhasTabuleiro) geraListaPosicoesTabuleiro) | numLin <- [0..getNumLinhasTabuleiro - 1]]

getCasasTabuleiroVoltaDuas :: [Posicao]
getCasasTabuleiroVoltaDuas = [(13,9), (9,3), (7,13), (3,7)]

getPosicaoBaseInicial :: Cor -> Posicao
getPosicaoBaseInicial c
    | c == Amarelo = (14, 6)
    | c == Vermelho = (6, 2)
    | c == Verde = (2, 10)
    | c == Azul = (10, 14)
    | otherwise = (0,0) -- cor inválida

getPosicaoBaseFinal :: Cor -> Posicao
getPosicaoBaseFinal c
    | c == Amarelo = (9, 8)
    | c == Vermelho = (8, 7)
    | c == Verde = (7, 8)
    | c == Azul = (8, 9)
    | otherwise = (0,0) -- cor inválida

posicaoDeBaseInicial :: Posicao -> Bool
posicaoDeBaseInicial posi = posi `elem` [(6,2), (2,10), (10,14), (14,6)]

posicaoDeBaseFinal :: Posicao -> Bool
posicaoDeBaseFinal posi = posi `elem` [(8,7), (7,8), (8,9), (9,8)]

posicaoTerritorioAmarelo :: Posicao -> Bool
posicaoTerritorioAmarelo posi = posi `elem` [(14, 6), (14, 7)] ++ [(lin, 8) | lin <- [9..14]]

posicaoTerritorioVermelho :: Posicao -> Bool
posicaoTerritorioVermelho posi = posi `elem` [(6, 2), (7, 2)] ++ [(8, col) | col <- [2..7]]

posicaoTerritorioVerde :: Posicao -> Bool
posicaoTerritorioVerde posi = posi `elem` [(2, 10), (2, 9)] ++ [(lin, 8) | lin <- [2..7]]

posicaoTerritorioAzul :: Posicao -> Bool
posicaoTerritorioAzul posi = posi `elem` [(10, 14), (9, 14)] ++ [(8, col) | col <- [9..14]]

posicaoDeMovimentacao :: Posicao -> Bool
posicaoDeMovimentacao (lin, col) =
    (lin, col) `notElem` [(8, 8), (7, 7), (9, 9), (9, 7), (7, 9)] && 
    (lin `elem` [7..9] || col `elem` [7..9]) || 
    posicaoDeBaseInicial (lin, col)

geraTabuleiroVazio :: Tabuleiro
geraTabuleiroVazio = Map.fromList ([(posi, []) | posi <- geraListaPosicoesTabuleiro])

toStringTabuleiroComCor :: Posicao -> String -> String
toStringTabuleiroComCor posi str 
    | posicaoTerritorioAmarelo posi = setColor str Amarelo
    | posicaoTerritorioVermelho posi = setColor str Vermelho
    | posicaoTerritorioVerde posi = setColor str Verde
    | posicaoTerritorioAzul posi = setColor str Azul
    | otherwise = str

-- printa as casas do tabuleiro delimitando com colchetes
toStringCasasTabuleiro :: [Posicao] -> Tabuleiro -> String
toStringCasasTabuleiro [] _ = ""
toStringCasasTabuleiro ((lin, col):t) tab
            | posicaoDeMovimentacao (lin, col) = do
                let nomesPecas = getNomesPecasCasaTabuleiro (lin, col) tab
                let lenNomesPeca = getNumPecasCasaTabuleiro (lin, col) tab

                if lenNomesPeca == 0 then
                    if (lin, col) `elem` getCasasTabuleiroVoltaDuas
                        then setColorMagenta "[   -2   ]" ++ toStringCasasTabuleiro t tab
                        else toStringTabuleiroComCor (lin, col) "[        ]" ++ toStringCasasTabuleiro t tab
                    else if lenNomesPeca == 1 then toStringTabuleiroComCor (lin, col) "[   "++ nomesPecas ++ toStringTabuleiroComCor (lin, col) "   ]" ++ toStringCasasTabuleiro t tab
                    else if lenNomesPeca == 2 then toStringTabuleiroComCor (lin, col) "[  "++ nomesPecas ++ toStringTabuleiroComCor (lin, col) "  ]" ++ toStringCasasTabuleiro t tab
                    else if lenNomesPeca == 3 then toStringTabuleiroComCor (lin, col) "[ "++ nomesPecas ++ toStringTabuleiroComCor (lin, col) " ]" ++ toStringCasasTabuleiro t tab
                    else toStringTabuleiroComCor (lin, col) "["++ nomesPecas ++ toStringTabuleiroComCor (lin, col) "]" ++ toStringCasasTabuleiro t tab
            | otherwise = "          " ++ toStringCasasTabuleiro t tab

toStringTabuleiro :: [[Posicao]] -> Tabuleiro -> String
toStringTabuleiro [] _ = ""
toStringTabuleiro (listPosi:t) tab = toStringCasasTabuleiro listPosi tab ++ "\n" ++ toStringTabuleiro t tab
