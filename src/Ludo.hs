module Ludo where

import System.IO
import Data.Char
import System.Random

import Util
import Tipos
import Peca
import Tabuleiro

mudaJogadorDaVez :: Jogador -> Jogador -> Jogador -> Dado -> Tabuleiro -> Jogador
mudaJogadorDaVez jog1 jog2 jogVez dado tab
 | verificaJogadorVenceu jog1 tab || verificaJogadorVenceu jog2 tab || dado == 6 = jogVez -- se o jogador venceu ou ele tirou 6 ele continua na vez
 | jogVez == jog1 = jog2 -- se o jogador da vez é o jog1 mude para o jog2
 | otherwise = jog1 -- se o jogador da vez é o jog2 mude para o jog2


menuMovimentaPeca :: Tabuleiro -> Jogador -> Jogador -> Jogador -> Dado -> IO Tabuleiro
menuMovimentaPeca tab jog1 jog2 jogVez dado = do
  let listaPecas = getListaPecasJogaveis (pecasJogador jogVez tab) dado tab
  if not (null listaPecas)
    then do
      cls -- limpa a tela
      putStrLn "-----------------------------------------------------------------------------------\n"
      putStrLn ("Jogador: " ++ show(corJogador jogVez))
      putStrLn ("Dado: " ++ show dado)
      putStrLn "-----------------------------------------------------------------------------------\n"
      putStrLn (printTabuleiro (geraMatrizPosicoesTabuleiro 15 15) tab)
      putStrLn "-------------------------------- Escolher Peça ------------------------------------\n"
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
          
          runLudo tabPecaMovida jog1 jog2 (mudaJogadorDaVez jog1 jog2 jogVez dado tabPecaMovida)
        else do
          putStrLn "Opção inválida, tente novamente\n"
          putStrLn "Pressione <Enter> para voltar ao menu anterior"
          getChar
          menuMovimentaPeca tab jog1 jog2 jogVez dado
    else do
      cls -- limpa a tela
      putStrLn "-----------------------------------------------------------------------------------\n"
      putStrLn ("Jogador: " ++ show(corJogador jogVez))
      putStrLn ("Dado: " ++ show dado)
      putStrLn "-----------------------------------------------------------------------------------\n"
      putStrLn (printTabuleiro (geraMatrizPosicoesTabuleiro 15 15) tab)
      putStrLn "-------------------------------- Escolher Peça ------------------------------------\n"
      putStrLn "sem opções de peça \n"
      putStrLn "Pressione <Enter> para voltar ao menu anterior"
      getChar
      runLudo tab jog1 jog2 (mudaJogadorDaVez jog1 jog2 jogVez dado tab)

executarOpcao :: Tabuleiro -> Jogador -> Jogador -> Jogador -> Char -> IO Tabuleiro
executarOpcao tab jog1 jog2 jogVez '1' = do
  gen <- newStdGen -- obtém um novo gerador aleatório para ser usado em funções random 
  let dado = fst (randomR (1,6) gen :: (Int, StdGen)) -- gerando valor aleatorio de 1 a 6
  menuMovimentaPeca tab jog1 jog2 jogVez dado
executarOpcao tab jog1 jog2 jogVez '2' = do
  putStrLn "A Desenvolver"
  return tab
executarOpcao tab _ _ _ '3' =
  return tab
executarOpcao tab jog1 jog2 jogVez _ = do
  putStrLn "\nOpção inválida! Tente novamente..."
  putStrLn "Pressione <Enter> para voltar ao menu anterior"
  getChar
  runLudo tab jog1 jog2 jogVez

verificaJogadorVenceu :: Jogador -> Tabuleiro -> Bool
verificaJogadorVenceu jog tab
 | sum[length (listaMovimentosVitoria p) | p <- pecasJogador jog tab] == 0 = True
 | otherwise = False

clear = putStr "\ESC[2J"

runLudo :: Tabuleiro -> Jogador -> Jogador -> Jogador -> IO Tabuleiro
runLudo tab jog1 jog2 jogVez = do
  cls -- limpa a tela
  putStrLn "-----------------------------------------------------------------------------------\n"
  putStrLn ("Jogador: " ++ show(corJogador jogVez))
  putStrLn "-----------------------------------------------------------------------------------\n"
  putStrLn "-------------------------------- Ludo ---------------------------------------------\n"
  putStrLn (printTabuleiro (geraMatrizPosicoesTabuleiro 15 15) tab)
  if verificaJogadorVenceu jogVez tab
    then do
      putStrLn ("Vitória do jogador " ++ show(corJogador jogVez))
      putStrLn "Pressione <Enter> para voltar ao menu principal"
      getChar
      return tab
    else do
      putStrLn "(1) Jogar Dado"
      putStrLn "(2) Salvar jogo e voltar"
      putStrLn "(3) Voltar"
      putStrLn "-----\nOpção: "
      op <- getChar
      getChar -- descarta o Enter
      if jogVez == jog1
        then executarOpcao tab jog1 jog2 jog1 op
        else executarOpcao tab jog1 jog2 jog2 op
