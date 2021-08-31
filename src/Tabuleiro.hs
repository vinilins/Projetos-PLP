module Tabuleiro where

import qualified Data.Map as Map
import Tipos
import Util
import Data.List

-- recebe uma chave que eh uma posicao e retorna o valor que eh a Casa do Tabuleiro
getCasaTabuleiro :: Posicao -> Tabuleiro -> CasaTabuleiro
getCasaTabuleiro posi tab = tab Map.! posi

-- retorna uma string comtendo o nome de todas as peças que estão na casa do tabuleiro
getNomesPecasCasaTabuleiro :: Posicao -> Tabuleiro -> String
getNomesPecasCasaTabuleiro posi tab = intercalate "" [setColor (nomePeca x) (corPeca x) | x <- getCasaTabuleiro posi tab]

getNumPecasCasaTabuleiro :: Posicao -> Tabuleiro -> Int
getNumPecasCasaTabuleiro posi tab = length (getCasaTabuleiro posi tab)

-- adiciona uma casa ou modifica a casa existente num tabuleiro
adicionaCasaTabuleiro :: CasaTabuleiro -> Posicao -> Tabuleiro -> Tabuleiro
adicionaCasaTabuleiro casaTab posi = Map.insert posi casaTab

-- gera a lista com todas as posicoes do tabuleiro (gera de 1 a numLin linhas, 1 a numCol colunas e faz a concatenacao para gerar o produto cartesiano)
geraListaPosicoesTabuleiro :: NumLinhas -> NumColunas -> [Posicao]
geraListaPosicoesTabuleiro numLin numCol = [(lin, col) :: Posicao | lin <- [1..numLin], col <- [1..numCol]]

-- organiza a lista de posicoes em formato de matriz
geraMatrizPosicoesTabuleiro :: NumLinhas -> NumColunas -> [[Posicao]]
geraMatrizPosicoesTabuleiro numLin numCol = [take numLin (drop (x * numLin) (geraListaPosicoesTabuleiro numLin numCol)) | x <- [0..numLin-1]]

getCasasTabuleiroVoltaDuas :: [Posicao]
getCasasTabuleiroVoltaDuas = [(13,9), (9,3), (7,13), (3,7)]

getPosicaoBaseInicial :: Cor -> Posicao
getPosicaoBaseInicial c
    | c == Amarelo = (14, 6)
    | c == Vermelho = (6, 2)
    | c == Verde = (2, 10)
    | c == Azul = (10, 14)
    | otherwise = (0,0) -- cor inválida

posicaoDeBaseInicial :: Posicao -> Bool
posicaoDeBaseInicial posi = posi `elem` [(6,2), (2,10), (10,14), (14,6)]

posicaoDeBaseFinal :: Posicao -> Bool
posicaoDeBaseFinal posi = posi `elem` [(8,7), (7,8), (8,9), (9,8)]

posicaoTerritorioAmarelo :: Posicao -> Bool
posicaoTerritorioAmarelo posi = posi `elem` [(14, 6), (14, 7)] ++ [(x, 8) | x <- [9..14]]

posicaoTerritorioVermelho :: Posicao -> Bool
posicaoTerritorioVermelho posi = posi `elem` [(6, 2), (7, 2)] ++ [(8, x) | x <- [2..7]]

posicaoTerritorioVerde :: Posicao -> Bool
posicaoTerritorioVerde posi = posi `elem` [(2, 10), (2, 9)] ++ [(x, 8) | x <- [2..7]]

posicaoTerritorioAzul :: Posicao -> Bool
posicaoTerritorioAzul posi = posi `elem` [(10, 14), (9, 14)] ++ [(8, x) | x <- [9..14]]

getCasaTabuleiroComCor :: Posicao -> String -> String
getCasaTabuleiroComCor posi str 
 | posicaoTerritorioAmarelo posi = setColor str Amarelo
 | posicaoTerritorioVermelho posi = setColor str Vermelho
 | posicaoTerritorioVerde posi = setColor str Verde
 | posicaoTerritorioAzul posi = setColor str Azul
 | otherwise = str

posicaoDeMovimentacao :: Posicao -> Bool
posicaoDeMovimentacao (lin, col) =
    (lin, col) `notElem` [(8, 8), (7, 7), (9, 9), (9, 7), (7, 9)] && (lin `elem` [7..9] || col `elem` [7..9]) || posicaoDeBaseInicial (lin, col)

geraTabuleiroVazio :: NumLinhas -> NumColunas -> Tabuleiro
geraTabuleiroVazio numLin numCol = Map.fromList ([(posicao, []) | posicao <- geraListaPosicoesTabuleiro numLin numCol])

-- printa as casas do tabuleiro delimitando com colchetes
printCasasTabuleiro :: [Posicao] -> Tabuleiro -> String
printCasasTabuleiro [] _ = ""
printCasasTabuleiro ((lin, col):t) tab
            | posicaoDeMovimentacao (lin, col) = do
                let nomesPecas = getNomesPecasCasaTabuleiro (lin, col) tab
                let lenNomesPeca = getNumPecasCasaTabuleiro (lin, col) tab

                if lenNomesPeca == 0 then
                    if (lin, col) `elem` getCasasTabuleiroVoltaDuas
                        then setColorMagenta "[   -2   ]" ++ printCasasTabuleiro t tab
                        else getCasaTabuleiroComCor (lin, col) "[        ]" ++ printCasasTabuleiro t tab
                    else if lenNomesPeca == 1 then getCasaTabuleiroComCor (lin, col) "[   "++ nomesPecas ++ getCasaTabuleiroComCor (lin, col) "   ]" ++ printCasasTabuleiro t tab
                    else if lenNomesPeca == 2 then getCasaTabuleiroComCor (lin, col) "[  "++ nomesPecas ++ getCasaTabuleiroComCor (lin, col) "  ]" ++ printCasasTabuleiro t tab
                    else if lenNomesPeca == 3 then getCasaTabuleiroComCor (lin, col) "[ "++ nomesPecas ++ getCasaTabuleiroComCor (lin, col) " ]" ++ printCasasTabuleiro t tab
                    else getCasaTabuleiroComCor (lin, col) "["++ nomesPecas ++ getCasaTabuleiroComCor (lin, col) "]" ++ printCasasTabuleiro t tab
            | otherwise = "          " ++ printCasasTabuleiro t tab

printTabuleiro :: [[Posicao]] -> Tabuleiro -> String
printTabuleiro [] _ = ""
printTabuleiro (listPosi:t) tab = printCasasTabuleiro listPosi tab ++ "\n" ++ printTabuleiro t tab
