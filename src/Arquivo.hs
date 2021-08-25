{-# LANGUAGE OverloadedStrings #-}

module Arquivo where

import Data.Aeson as Json
import qualified Data.ByteString.Lazy as BS (readFile, writeFile)
import System.IO
import qualified Data.Map as Map

import Util
import Ludo
import Tipos
import Peca
import Tabuleiro

pathFileData :: String -> String
pathFileData nameArq = "data/" ++ nameArq 

salvarLudo :: Jogador -> Jogador -> Jogador -> Tabuleiro -> IO()
salvarLudo jog1 jog2 jogVez tab = do
    BS.writeFile (pathFileData "jogador1.json") (Json.encode jog1)
    BS.writeFile (pathFileData "jogador2.json") (Json.encode jog2)
    BS.writeFile (pathFileData "jogadorVez.json") (Json.encode jogVez)
    BS.writeFile (pathFileData "tabuleiro.json") (Json.encode tab)

    putStrLn "\n Jogo salvo com sucesso"
    putStrLn "Pressione <Enter> para voltar\n" 
    --getChar -- descarta o enter 


{-
carregarLudo :: IO (Jogador, Jogador, Jogador, Tabuleiro) 
carregarLudo = do 
    inputJog1 <- BS.readFile (pathFileData "jogador1.json")
    inputJog2 <- BS.readFile (pathFileData "jogador2.json")
    inputJogVez <- BS.readFile (pathFileData "jogadorVez.json")
    inputTabuleiro <- BS.readFile (pathFileData "tabuleiro.json")
    
    let jog1 = Json.decode inputJog1 :: Maybe Jogador
    let jog2 = Json.decode inputJog1 :: Maybe Jogador
    let jogVez = Json.decode inputJog1 :: Maybe Jogador
    let tabuleiro = Json.decode inputJog1 :: Maybe Tabuleiro
    
    
    return (jog1, jog2, jogVez, tabuleiro) -- precisa verificar cada um

    
    case mm of
        Nothing -> print "error parsing JSON"
        Just m -> print (nomeJogador m)
-}



mainArquivo :: IO()
mainArquivo = do
    let pecaAmarelo01 = Peca Amarelo "Amarelo 1" (getListaMovimentosVitoria Amarelo)
    let pecaAmarelo02 = Peca Amarelo "Amarelo 2" (getListaMovimentosVitoria Amarelo)
    let pecaAmarelo03 = Peca Amarelo "Amarelo 3" (getListaMovimentosVitoria Amarelo)
    let pecaAmarelo04 = Peca Amarelo "Amarelo 4" (getListaMovimentosVitoria Amarelo)

    let pecaVerde01 = Peca Verde "Verde 1" (getListaMovimentosVitoria Verde)
    let pecaVerde02 = Peca Verde "Verde 2" (getListaMovimentosVitoria Verde)
    let pecaVerde03 = Peca Verde "Verde 3" (getListaMovimentosVitoria Verde)
    let pecaVerde04 = Peca Verde "Verde 4" (getListaMovimentosVitoria Verde)

    let jogador1 = Jogador Amarelo "Pedro"
    let jogador2 = Jogador Verde "Bot"

    let casaTabuleiroBaseJogador1 = [pecaAmarelo01]
    let posicaoBaseJogador1 =  getPosicaoBaseInicial Amarelo
  
    let casaTabuleiroBaseJogador2 = [pecaVerde01]
    let posicaoBaseJogador2 = getPosicaoBaseInicial Verde

    let tabuleiro = adicionaCasaTabuleiro casaTabuleiroBaseJogador2 posicaoBaseJogador2 (adicionaCasaTabuleiro casaTabuleiroBaseJogador1 posicaoBaseJogador1 (geraTabuleiroVazio 15 15))

    -- Testes
    
    --BS.writeFile (pathArquivos ++ "jogador1.json") (Json.encode jogador1)
    --BS.writeFile (pathArquivos ++ "jogador2.json") (Json.encode jogador2)
    --BS.writeFile (pathArquivos ++ "jogadorVez.json") (Json.encode jogador1)
    BS.writeFile (pathFileData "tabuleiro.json") (Json.encode tabuleiro)  

    putStrLn "\n"


    print (tabuleiro)
    
    putStrLn "\n"

    input <- BS.readFile (pathFileData "tabuleiro.json")
    let mm = Json.decode input :: Maybe Tabuleiro
    case mm of
        Nothing -> print "error parsing JSON"
        Just m -> putStrLn (printTabuleiro (geraMatrizPosicoesTabuleiro 15 15)  m)
  
    putStrLn "\n"
    
