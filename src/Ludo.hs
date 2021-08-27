{-# LANGUAGE OverloadedStrings #-}

module Ludo where

import System.IO
import Data.Char
import System.Random
import Data.Aeson as Json
import qualified Data.ByteString.Lazy as BS (readFile, writeFile)
import System.Directory

import Util
import Tipos
import Peca
import Tabuleiro

getNomePastaArquivos :: String
getNomePastaArquivos = "data/"

getCaminhoJogador :: FilePath
getCaminhoJogador = getNomePastaArquivos ++ "jogador1.json"

getCaminhoBot :: FilePath
getCaminhoBot = getNomePastaArquivos ++ "jogador2.json"

getCaminhoJogadorVez :: FilePath
getCaminhoJogadorVez = getNomePastaArquivos ++ "jogadorVez.json"

getCaminhoTabuleiro :: FilePath
getCaminhoTabuleiro = getNomePastaArquivos ++ "tabuleiro.json"

salvarJogo :: Tabuleiro -> Jogador -> Jogador -> Jogador -> IO Tabuleiro
salvarJogo tab jog1 jog2 jogVez = do
    BS.writeFile getCaminhoJogador (Json.encode jog1)
    BS.writeFile getCaminhoBot (Json.encode jog2)
    BS.writeFile getCaminhoJogadorVez (Json.encode jogVez)
    BS.writeFile getCaminhoTabuleiro (Json.encode tab)

    putStrLn "\nJogo salvo com sucesso, Pressione <Enter> para voltar\n"
    getChar -- descarta o enter
    menuLudo tab jog1 jog2 jogVez

iniciarJogoSalvo :: Tabuleiro -> Jogador -> Jogador -> Jogador -> IO Tabuleiro
iniciarJogoSalvo tab jog1 jog2 jogVez = do
  existeJogador <- doesFileExist getCaminhoJogador
  existeBot <- doesFileExist getCaminhoBot
  existeJogadorVez <- doesFileExist getCaminhoJogadorVez
  existeTabuleiro <- doesFileExist getCaminhoTabuleiro

  if existeJogador && existeBot && existeJogadorVez && existeTabuleiro
    then do
      inputJog1 <- BS.readFile getCaminhoJogador
      inputJog2 <- BS.readFile getCaminhoBot
      inputJogVez <- BS.readFile getCaminhoJogadorVez
      inputTabuleiro <- BS.readFile getCaminhoTabuleiro
      
      let (Just jog1) = Json.decode inputJog1 :: Maybe Jogador
      let (Just jog2) = Json.decode inputJog2 :: Maybe Jogador
      let (Just jogVez) = Json.decode inputJogVez :: Maybe Jogador
      let (Just tab) = Json.decode inputTabuleiro :: Maybe Tabuleiro
      
      iniciarJogo tab jog1 jog2 jogVez
    else do
      putStrLn "\nNão existe nenhum jogo salvo, Pressione <Enter> para voltar\n" 
      getChar -- descarta o Enter
      menuLudo tab jog1 jog2 jogVez

verificaJogadorVenceu :: Jogador -> Tabuleiro -> Bool
verificaJogadorVenceu jog tab
 | sum[length (listaMovimentosVitoria p) | p <- pecasJogador jog tab] == 0 = True
 | otherwise = False

mudaJogadorDaVez :: Jogador -> Jogador -> Jogador -> Dado -> Tabuleiro -> Jogador
mudaJogadorDaVez jog1 jog2 jogVez dado tab
 | verificaJogadorVenceu jog1 tab || verificaJogadorVenceu jog2 tab || dado == 6 = jogVez -- se o jogador venceu ou ele tirou 6 ele continua na vez
 | jogVez == jog1 = jog2 -- se o jogador da vez é o jog1 mude para o jog2
 | otherwise = jog1 -- se o jogador da vez é o jog2 mude para o jog2

printMenuJogador :: Tabuleiro -> Jogador -> Dado -> IO Tabuleiro
printMenuJogador tab jog dado = do
  putStrLn "-----------------------------------------------------------------------------------\n"
  putStrLn ("Jogador: " ++ show(corJogador jog))
  putStrLn ("Dado: " ++ show dado)
  putStrLn "-----------------------------------------------------------------------------------\n"
  putStrLn (printTabuleiro (geraMatrizPosicoesTabuleiro 15 15) tab)
  putStrLn "-------------------------------- Escolher Peça ------------------------------------\n"
  return tab

menuMovimentaPeca :: Tabuleiro -> Jogador -> Jogador -> Jogador -> Dado -> IO Tabuleiro
menuMovimentaPeca tab jog1 jog2 jogVez dado = do
  let listaPecas = getListaPecasJogaveis (pecasJogador jogVez tab) dado tab
  if not (null listaPecas)
    then do
      cls -- limpa a tela
      printMenuJogador tab jogVez dado
      putStrLn (printListaPecas (reverse listaPecas))
      putStrLn "-----\nOpção: "
      op <- getChar
      getChar -- descarta o Enter
      if op `elem` ['1'.. intToDigit (length listaPecas)]
        then do
          let peca = listaPecas !! (digitToInt op - 1)
          let tabPecaMovida = 
                if posicaoDeBaseInicial (getPosicaoPeca peca tab) && dado == 6 -- se a peça está na sua base e o jogador tirou 6 
                  then movimentaPecaRepetidamente peca 1 tab -- o movimento que deve ser executado é o de tirar a peça da base
                  else movimentaPecaRepetidamente peca dado tab -- executa os movimentos normalmente de acordo com o valor do dado
            
          print (listaMovimentosVitoria peca) -- Só de teste
          getChar -- descarta o Enter - Só de teste
          
          iniciarJogo tabPecaMovida jog1 jog2 (mudaJogadorDaVez jog1 jog2 jogVez dado tabPecaMovida)
        else do
          putStrLn "\nOpção inválida, Pressione <Enter> para voltar\n" 
          getChar
          menuMovimentaPeca tab jog1 jog2 jogVez dado
    else do
      cls -- limpa a tela
      printMenuJogador tab jogVez dado
      putStrLn "\nSem opções de peça, Pressione <Enter> para voltar\n"
      getChar
      iniciarJogo tab jog1 jog2 (mudaJogadorDaVez jog1 jog2 jogVez dado tab)

executarOpcaoJogo :: Tabuleiro -> Jogador -> Jogador -> Jogador -> Char -> IO Tabuleiro
executarOpcaoJogo tab jog1 jog2 jogVez '1' = do
  gen <- newStdGen -- obtém um novo gerador aleatório para ser usado em funções random 
  let dado = fst (randomR (1,6) gen :: (Int, StdGen)) -- gerando valor aleatorio de 1 a 6
  menuMovimentaPeca tab jog1 jog2 jogVez dado
executarOpcaoJogo tab jog1 jog2 jogVez '2' =
  menuLudo tab jog1 jog2 jogVez
executarOpcaoJogo tab jog1 jog2 jogVez _ = do
  putStrLn "\nOpção inválida, Pressione <Enter> para voltar\n"  
  getChar -- descarta o enter
  iniciarJogo tab jog1 jog2 jogVez

iniciarJogo :: Tabuleiro -> Jogador -> Jogador -> Jogador -> IO Tabuleiro
iniciarJogo tab jog1 jog2 jogVez = do
  cls -- limpa a tela
  putStrLn "-----------------------------------------------------------------------------------\n"
  putStrLn ("Jogador: " ++ show(corJogador jogVez))
  putStrLn "-----------------------------------------------------------------------------------\n"
  putStrLn "-------------------------------- Ludo ---------------------------------------------\n"
  putStrLn (printTabuleiro (geraMatrizPosicoesTabuleiro 15 15) tab)
  if verificaJogadorVenceu jogVez tab
    then do
      putStrLn ("Vitória do jogador " ++ show(corJogador jogVez))
      putStrLn "Pressione <Enter> para voltar"
      getChar
      return tab
    else do
      putStrLn "(1) Jogar Dado"
      putStrLn "(2) Voltar"
      putStrLn "-----\nOpção: "
      op <- getChar
      getChar -- descarta o Enter
      if jogVez == jog1
        then executarOpcaoJogo tab jog1 jog2 jog1 op
        else executarOpcaoJogo tab jog1 jog2 jog2 op

getNovoJogo :: (Tabuleiro, Jogador, Jogador, Jogador)
getNovoJogo = do
    let pecaAmarelo01 = Peca Amarelo "A1" (getListaMovimentosVitoria Amarelo)
    let pecaAmarelo02 = Peca Amarelo "A2" (getListaMovimentosVitoria Amarelo)
    let pecaAmarelo03 = Peca Amarelo "A3" (getListaMovimentosVitoria Amarelo)
    let pecaAmarelo04 = Peca Amarelo "A4" (getListaMovimentosVitoria Amarelo)

    let pecaVerde01 = Peca Verde "V1" (getListaMovimentosVitoria Verde)
    let pecaVerde02 = Peca Verde "V2" (getListaMovimentosVitoria Verde)
    let pecaVerde03 = Peca Verde "V3" (getListaMovimentosVitoria Verde)
    let pecaVerde04 = Peca Verde "V4" (getListaMovimentosVitoria Verde)

    let jogador1 = Jogador Amarelo "Pedro"
    let jogador2 = Jogador Verde "Bot"

    let casaTabuleiroBaseJogador1 = [pecaAmarelo01, pecaAmarelo02, pecaAmarelo03, pecaAmarelo04]
    let posicaoBaseJogador1 =  getPosicaoBaseInicial Amarelo
  
    let casaTabuleiroBaseJogador2 = [pecaVerde01, pecaVerde02, pecaVerde03, pecaVerde04]
    let posicaoBaseJogador2 = getPosicaoBaseInicial Verde

    let tabuleiro = adicionaCasaTabuleiro casaTabuleiroBaseJogador2 posicaoBaseJogador2 (adicionaCasaTabuleiro casaTabuleiroBaseJogador1 posicaoBaseJogador1 (geraTabuleiroVazio 15 15))

    (tabuleiro, jogador1, jogador2, jogador1)

executarOpcaoMenuLudo :: Tabuleiro -> Jogador -> Jogador -> Jogador -> Char -> IO Tabuleiro
executarOpcaoMenuLudo tab jog1 jog2 jogVez '1' = do
  let (newTab, newJog1, newJog2, newJogVez) = getNovoJogo
  iniciarJogo newTab newJog1 newJog2 newJogVez
executarOpcaoMenuLudo tab jog1 jog2 jogVez '2' = do
  iniciarJogo tab jog1 jog2 jogVez
executarOpcaoMenuLudo tab jog1 jog2 jogVez '3' = do
  salvarJogo tab jog1 jog2 jogVez
executarOpcaoMenuLudo tab jog1 jog2 jogVez '4' = do
  iniciarJogoSalvo tab jog1 jog2 jogVez
executarOpcaoMenuLudo tab jog1 jog2 jogVez '5' = do
  return tab
executarOpcaoMenuLudo tab jog1 jog2 jogVez _ = do
  putStrLn "\nOpção inválida, Pressione <Enter> para voltar\n"  
  getChar -- descarta o enter
  menuLudo tab jog1 jog2 jogVez

menuLudo :: Tabuleiro -> Jogador -> Jogador -> Jogador -> IO Tabuleiro
menuLudo tab jog1 jog2 jogVez = do
    cls
    putStrLn ludoLogo
    putStrLn "(1) Novo Jogo"
    putStrLn "(2) Continuar Jogo"
    putStrLn "(3) Salvar Jogo"
    putStrLn "(4) Continuar Jogo Salvo"
    putStrLn "(5) Voltar"
    putStrLn "-----\nOpção: "
    op <- getChar
    getChar -- descarta o Enter
    executarOpcaoMenuLudo tab jog1 jog2 jogVez op

iniciarMenuLudo :: IO()
iniciarMenuLudo = do
  let (newTab, newJog1, newJog2, newJogVez) = getNovoJogo
  menuLudo newTab newJog1 newJog2 newJogVez
  return ()

