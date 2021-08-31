module Bot where

import Data.List

import Tipos
import Util
import Peca
import Tabuleiro

{-

Avaliações para um tabuleiro

se a lista só tem uma peça, jogue ela

Se o tab tem menos peças minha na base = +25 -- tirou uma peça da base

Se o tab tem mais peças adversárias na base = +45 -- comeu a peça de um adversário

se duas pecas minhas ficarem na mesma posicao = +65 -- criou um bloqueio

se o tab tem mais pecas minhas na base final = +80 -- colocou uma peca na base final

-}

-- Cada função de avaliação vai comparar o tabuleiro no momento com o tabuleiro possivel, se o tabuleiro possivel passar na avaliação é retornado o peso da avaliação 


avaliaComeuPecaDoAdversario :: Jogador -> Jogador -> Tabuleiro -> Tabuleiro -> Int
avaliaComeuPecaDoAdversario jogBot jogAdv tab tabPos = do
    let posicaoBaseInicialAdv = getPosicaoBaseInicial (corJogador jogAdv)
    let numPecasBaseInicialAdvTab = getNumPecasCasaTabuleiro posicaoBaseInicialAdv tab
    let numPecasBaseInicialAdvTabJog = getNumPecasCasaTabuleiro posicaoBaseInicialAdv tabPos

    if numPecasBaseInicialAdvTabJog > numPecasBaseInicialAdvTab
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

avaliaJogada :: Jogador -> Jogador -> Tabuleiro -> Tabuleiro -> Int
avaliaJogada jogBot jogAdv tab tabPos = 
    avaliaRetirouPecaDaBaseInicial jogBot jogAdv tab tabPos +
    avaliaComeuPecaDoAdversario jogBot jogAdv tab tabPos

avaliaJogadas :: Jogador -> Jogador -> Tabuleiro -> [Tabuleiro] -> [Int]
avaliaJogadas jogBot jogAdv tab listaTabPos = [avaliaJogada jogBot jogAdv tab tabJog | tabJog <- listaTabPos]

geraListaTabuleirosPossiveis :: [Peca] -> NumDado -> Tabuleiro -> [Tabuleiro]
geraListaTabuleirosPossiveis pecas numDado tab = [movimentaPecaRepetidamente x numDado tab | x <- pecas]   

decideJogadaBot2 :: Jogador -> Jogador -> [Peca] -> NumDado -> Tabuleiro -> IO Peca
decideJogadaBot2 jogBot jogAdv pecas numDado tab = do
    if length pecas == 1
        then do
            putStrLn "Somente uma peça disponivel, joga ela"
            return (head pecas)
        else do
            let listaTabPossiveis = geraListaTabuleirosPossiveis pecas numDado tab
            let listaAvaliacaoJogadas = avaliaJogadas jogBot jogAdv tab listaTabPossiveis 

            let pecaEscolhida = pecas !! head (elemIndices (maximum listaAvaliacaoJogadas) listaAvaliacaoJogadas)

            print listaAvaliacaoJogadas
            print $ head (elemIndices (maximum listaAvaliacaoJogadas) listaAvaliacaoJogadas)
            getChar
    
            return pecaEscolhida

-- Joga a primeira peça disponivel
decideJogadaBot :: [Peca] -> NumDado -> Tabuleiro -> Peca
decideJogadaBot (h:t) numDado tab = h