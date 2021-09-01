module Bot where

import Data.List

import Tipos
import Util
import Peca
import Tabuleiro

-- Cada função de avaliação vai comparar o tabuleiro no momento com o tabuleiro possível, se o tabuleiro possível passar na avaliação é retornado o peso da avaliação 

avaliaColocouPecaNaBaseFinal :: Jogador -> Jogador -> Tabuleiro -> Tabuleiro -> Int
avaliaColocouPecaNaBaseFinal jogBot jogAdv tab tabPos = do
    let posicaoBaseFinal = getPosicaoBaseFinal (corJogador jogBot)
    let numPecasBaseFinalTab = getNumPecasCasaTabuleiro posicaoBaseFinal tab
    let numPecasBaseFinalTabPos = getNumPecasCasaTabuleiro posicaoBaseFinal tabPos
    
    if numPecasBaseFinalTabPos > numPecasBaseFinalTab
        then 65 -- Peso da avaliação
        else 0

avaliaComeuPecaDoAdversario :: Jogador -> Jogador -> Tabuleiro -> Tabuleiro -> Int
avaliaComeuPecaDoAdversario jogBot jogAdv tab tabPos = do
    let posicaoBaseInicialAdv = getPosicaoBaseInicial (corJogador jogAdv)
    let numPecasBaseInicialAdvTab = getNumPecasCasaTabuleiro posicaoBaseInicialAdv tab
    let numPecasBaseInicialAdvTabPos = getNumPecasCasaTabuleiro posicaoBaseInicialAdv tabPos

    if numPecasBaseInicialAdvTabPos > numPecasBaseInicialAdvTab
        then 45 -- Peso da avaliação
        else 0

avaliaRetirouPecaDaBaseInicial :: Jogador -> Jogador -> Tabuleiro -> Tabuleiro -> Int
avaliaRetirouPecaDaBaseInicial jogBot jogAdv tab tabPos = do
    let posicaoBaseInicial = getPosicaoBaseInicial (corJogador jogBot)
    let numPecasBaseInicialTab = getNumPecasCasaTabuleiro posicaoBaseInicial tab
    let numPecasBaseInicialTabJog = getNumPecasCasaTabuleiro posicaoBaseInicial tabPos
    
    if numPecasBaseInicialTabJog < numPecasBaseInicialTab
        then 25 -- Peso da avaliação
        else 0

avaliaPecaFicouBloqueadaOuNaoSeMoveu :: Jogador -> Jogador -> Tabuleiro -> Tabuleiro -> Int
avaliaPecaFicouBloqueadaOuNaoSeMoveu jogBot jogAdv tab tabPos = do
    if tab == tabPos
        then -70 -- Peso da avaliação
        else 0

avaliaJogada :: Jogador -> Jogador -> Tabuleiro -> Tabuleiro -> Int
avaliaJogada jogBot jogAdv tab tabPos = 
    avaliaRetirouPecaDaBaseInicial jogBot jogAdv tab tabPos +
    avaliaComeuPecaDoAdversario jogBot jogAdv tab tabPos +
    avaliaColocouPecaNaBaseFinal jogBot jogAdv tab tabPos +
    avaliaPecaFicouBloqueadaOuNaoSeMoveu jogBot jogAdv tab tabPos

avaliaJogadas :: Jogador -> Jogador -> Tabuleiro -> [Tabuleiro] -> [Int]
avaliaJogadas jogBot jogAdv tab listaTabPos = [avaliaJogada jogBot jogAdv tab tabJog | tabJog <- listaTabPos]

geraListaTabuleirosPossiveis :: [Peca] -> NumDado -> Tabuleiro -> [Tabuleiro]
geraListaTabuleirosPossiveis pecas numDado tab = [movimentaPecaRepetidamente x numDado tab | x <- pecas]   

decideJogadaBot :: Jogador -> Jogador -> [Peca] -> NumDado -> Tabuleiro -> IO Peca
decideJogadaBot jogBot jogAdv pecas numDado tab = do
    if length pecas == 1
        then do
            putStrLn "Somente uma peça disponivel, joga ela"
            return (head pecas)
        else do
            let listaTabPossiveis = geraListaTabuleirosPossiveis pecas numDado tab
            let listaAvaliacaoJogadas = avaliaJogadas jogBot jogAdv tab listaTabPossiveis
            let pecaEscolhida = pecas !! head (elemIndices (maximum listaAvaliacaoJogadas) listaAvaliacaoJogadas)
            return pecaEscolhida