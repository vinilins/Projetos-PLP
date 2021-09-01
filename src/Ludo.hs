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
import Bot
import Tabuleiro

getNomePastaArquivos :: FilePath
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

    putStrLn $ setColorGreen "\nJogo salvo com sucesso, Pressione <Enter> para voltar\n"
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

jogadorVenceu :: Jogador -> Tabuleiro -> Bool
jogadorVenceu jog tab = sum[length (listaMovimentosVitoria p) | p <- pecasJogador jog tab] == 0

mudaJogadorDaVez :: Jogador -> Jogador -> Jogador -> NumDado -> Tabuleiro -> Jogador
mudaJogadorDaVez jog1 jog2 jogVez numDado tab
    | jogadorVenceu jog1 tab || jogadorVenceu jog2 tab || numDado == 6 = jogVez -- se o jogador venceu ou ele tirou 6 ele continua na vez
    | jogVez == jog1 = jog2 -- se o jogador da vez é o jog1 mude para o jog2
    | otherwise = jog1 -- se o jogador da vez é o jog2 mude para o jog2

printMenuJogador :: Tabuleiro -> Jogador -> NumDado -> IO Tabuleiro
printMenuJogador tab jog numDado = do
    putStrLn "\n------------------------------------------------------------------------------------------------------------------------------------------------------------\n"
    putStrLn ("Jogador: " ++ printJogadorComCor jog) 
    putStrLn ("Dado: " ++ show numDado)
    putStrLn "\n------------------------------------------------------------------------- Tabuleiro ------------------------------------------------------------------------\n"
    putStrLn (printTabuleiro geraMatrizPosicoesTabuleiro tab)
    return tab

menuMovimentaPeca :: Tabuleiro -> Jogador -> Jogador -> Jogador -> NumDado -> IO Tabuleiro
menuMovimentaPeca tab jog1 jog2 jogVez numDado = do
    cls -- limpa a tela
    let listaPecas = getListaPecasJogaveis (pecasJogador jogVez tab) numDado tab
    if not(null listaPecas)
        then do
        if bot jogVez
            then do
            -- 'let peca = decideJogadaBot listaPecas numDado tab -- Normal
            peca <- decideJogadaBot2 jogVez (if jogVez == jog1 then jog2 else jog1) listaPecas numDado tab -- Para Testes
            let tabPecaMovida = 
                    if posicaoDeBaseInicial (getPosicaoPeca peca tab) && numDado == 6 -- se a peça está na sua base e o jogador tirou 6 
                    then movimentaPecaRepetidamente peca 1 tab -- o movimento que deve ser executado é o de tirar a peça da base
                    else movimentaPecaRepetidamente peca numDado tab -- executa os movimentos normalmente de acordo com o valor do dado          
            printMenuJogador tabPecaMovida jogVez numDado
            putStrLn ("O jogador " ++ printJogadorComCor jogVez ++ " moveu a peça " ++ printPecaComCor peca)
            putStrLn $ setColorGreen "\nPressione <Enter> para continuar\n"  
            getChar -- descarta o enter
            iniciarJogo tabPecaMovida jog1 jog2 (mudaJogadorDaVez jog1 jog2 jogVez numDado tabPecaMovida)
            else do
            printMenuJogador tab jogVez numDado
            putStrLn (printListaPecas (reverse listaPecas))
            putStrLn $ setColorGreen "-----\nOpção: "
            op <- getLine
            if length op == 1 && head op `elem` ['1'.. intToDigit (length listaPecas)]
                then do
                let peca = listaPecas !! (digitToInt (head op) - 1)
                let tabPecaMovida = 
                        if posicaoDeBaseInicial (getPosicaoPeca peca tab) && numDado == 6 -- se a peça está na sua base e o jogador tirou 6 
                        then movimentaPecaRepetidamente peca 1 tab -- o movimento que deve ser executado é o de tirar a peça da base
                        else movimentaPecaRepetidamente peca numDado tab -- executa os movimentos normalmente de acordo com o valor do dado          
                iniciarJogo tabPecaMovida jog1 jog2 (mudaJogadorDaVez jog1 jog2 jogVez numDado tabPecaMovida)
                else do
                    putStrLn $ setColorRed "\nOpção inválida, Pressione <Enter> para voltar\n" 
                    getChar -- descarta o enter
                    menuMovimentaPeca tab jog1 jog2 jogVez numDado
        else do
        printMenuJogador tab jogVez numDado
        if bot jogVez 
            then do
                putStrLn ("O jogador " ++ printJogadorComCor jogVez ++ " não moveu nenhuma peça")
                putStrLn $ setColorGreen "\nPressione <Enter> para continuar\n"  
            else 
                putStrLn $ setColorGreen "\nSem opções de peça, Pressione <Enter> para continuar\n"
        getChar -- descarta o enter
        iniciarJogo tab jog1 jog2 (mudaJogadorDaVez jog1 jog2 jogVez numDado tab)

executarOpcaoJogo :: Tabuleiro -> Jogador -> Jogador -> Jogador -> Opcao -> IO Tabuleiro
executarOpcaoJogo tab jog1 jog2 jogVez "1" = do
    gen <- newStdGen -- obtém um novo gerador aleatório para ser usado em funções random 
    let numDado = fst (randomR (1,6) gen :: (Int, StdGen)) -- gerando valor aleatorio de 1 a 6
    menuMovimentaPeca tab jog1 jog2 jogVez numDado
executarOpcaoJogo tab jog1 jog2 jogVez "2" =
    menuLudo tab jog1 jog2 jogVez
executarOpcaoJogo tab jog1 jog2 jogVez _ = do
    putStrLn $ setColorRed "\nOpção inválida, Pressione <Enter> para voltar\n"  
    getChar -- descarta o enter
    iniciarJogo tab jog1 jog2 jogVez

iniciarJogo :: Tabuleiro -> Jogador -> Jogador -> Jogador -> IO Tabuleiro
iniciarJogo tab jog1 jog2 jogVez = do
    if jogadorVenceu jogVez tab
        then do
            cls -- limpa a tela
            putStrLn "\n------------------------------------------------------------------------------------------------------------------------------------------------------------\n"
            putStrLn ("Jogador: " ++ printJogadorComCor jogVez) 
            putStrLn "\n------------------------------------------------------------------------- Tabuleiro ------------------------------------------------------------------------\n"
            putStrLn (printTabuleiro geraMatrizPosicoesTabuleiro tab)
            putStrLn ("Vitória do jogador " ++ printJogadorComCor jogVez)
            putStrLn $ setColorGreen "\nPressione <Enter> para voltar\n"
            getChar
            return tab
        else if bot jogVez
            then do
                if jogVez == jog1
                    then executarOpcaoJogo tab jog1 jog2 jog1 "1"
                    else executarOpcaoJogo tab jog1 jog2 jog2 "1"
        else do
            cls -- limpa a tela
            putStrLn "\n------------------------------------------------------------------------------------------------------------------------------------------------------------\n"
            putStrLn ("Jogador: " ++ printJogadorComCor jogVez) 
            putStrLn "\n------------------------------------------------------------------------- Tabuleiro ------------------------------------------------------------------------\n"
            putStrLn (printTabuleiro geraMatrizPosicoesTabuleiro tab)
            putStrLn $ setColorCiano "(1)" ++ " Jogar Dado"
            putStrLn $ setColorCiano "(2)" ++ " Voltar"
            putStrLn $ setColorGreen "-----\nOpção: "
            op <- getLine
            if jogVez == jog1
                then executarOpcaoJogo tab jog1 jog2 jog1 op
                else executarOpcaoJogo tab jog1 jog2 jog2 op

decideCorJogador :: Bool -> IO Cor 
decideCorJogador jogadorEscolheCor = do
    if jogadorEscolheCor
        then do
            cls
            putStrLn $ setColorRed ludoLogo
            putStrLn "\nDecida a sua cor: \n"
            putStrLn $ setColorCiano "(1)" ++ setColorYellow " Amarelo"
            putStrLn $ setColorCiano "(2)" ++ setColorRed " Vermelho"
            putStrLn $ setColorCiano "(3)" ++ setColorGreen " Verde"
            putStrLn $ setColorCiano "(4)" ++ setColorCiano " Azul"
            putStrLn $ setColorGreen "-----\nOpção: "
            op <- getLine

            if op == "1" then return Amarelo
            else if op == "2" then return Vermelho
            else if op == "3" then return Verde
            else if op == "4" then return Azul
            else do
                putStrLn $ setColorRed "\nOpção inválida, Pressione <Enter> para voltar\n"  
                getChar -- descarta o enter
                decideCorJogador jogadorEscolheCor
        else
            return Amarelo

criaCasaTabuleiroBaseJogador :: Cor -> CasaTabuleiro
criaCasaTabuleiroBaseJogador corJog = [Peca corJog (take 1 (show(corJog)) ++ show(x)) (getListaMovimentosVitoria corJog) | x <- [1..4]]

getNovoJogo :: Bool -> IO (Tabuleiro, Jogador, Jogador, Jogador)
getNovoJogo jogadorEscolheCor = do
    -- Cor do Jogador e do Bot
    corJog <- decideCorJogador jogadorEscolheCor
    let corJogBot = 
            case corJog of
            Amarelo -> Verde
            Verde -> Amarelo
            Vermelho -> Azul
            Azul -> Vermelho
    
    -- Jogador
    let jog = Jogador corJog jogadorNormal
    let casaTabuleiroBaseJog = criaCasaTabuleiroBaseJogador corJog
    let posicaoBaseJog = getPosicaoBaseInicial corJog

    -- Bot
    let jogBot = Jogador corJogBot jogadorBot
    let casaTabuleiroBaseJogBot = criaCasaTabuleiroBaseJogador corJogBot
    let posicaoBaseJogBot = getPosicaoBaseInicial corJogBot 

    -- Tabuleiro
    let tab = adicionaCasaTabuleiro casaTabuleiroBaseJogBot posicaoBaseJogBot (adicionaCasaTabuleiro casaTabuleiroBaseJog posicaoBaseJog geraTabuleiroVazio)

    return (tab, jog, jogBot, jog)

executarOpcaoMenuLudo :: Tabuleiro -> Jogador -> Jogador -> Jogador -> Opcao -> IO Tabuleiro
executarOpcaoMenuLudo tab jog1 jog2 jogVez "1" = do
    (newTab, newJog1, newJog2, newJogVez) <- getNovoJogo True
    iniciarJogo newTab newJog1 newJog2 newJogVez
executarOpcaoMenuLudo tab jog1 jog2 jogVez "2" = do
    iniciarJogo tab jog1 jog2 jogVez
executarOpcaoMenuLudo tab jog1 jog2 jogVez "3" = do
    salvarJogo tab jog1 jog2 jogVez
executarOpcaoMenuLudo tab jog1 jog2 jogVez "4" = do
    iniciarJogoSalvo tab jog1 jog2 jogVez
executarOpcaoMenuLudo tab jog1 jog2 jogVez "5" = do
    return tab
executarOpcaoMenuLudo tab jog1 jog2 jogVez _ = do
    putStrLn $ setColorRed "\nOpção inválida, Pressione <Enter> para voltar\n"  
    getChar -- descarta o enter
    menuLudo tab jog1 jog2 jogVez

menuLudo :: Tabuleiro -> Jogador -> Jogador -> Jogador -> IO Tabuleiro
menuLudo tab jog1 jog2 jogVez = do
    cls
    putStrLn $ setColorRed ludoLogo
    putStrLn $ setColorCiano "(1)" ++ " Novo Jogo"
    putStrLn $ setColorCiano "(2)" ++ " Continuar Jogo"
    putStrLn $ setColorCiano "(3)" ++ " Salvar Jogo"
    putStrLn $ setColorCiano "(4)" ++ " Continuar Jogo Salvo"
    putStrLn $ setColorCiano "(5)" ++ " Voltar"
    putStrLn $ setColorGreen "-----\nOpção: "
    op <- getLine
    executarOpcaoMenuLudo tab jog1 jog2 jogVez op

iniciarMenuLudo :: IO()
iniciarMenuLudo = do
    (newTab, newJog1, newJog2, newJogVez) <- getNovoJogo False
    menuLudo newTab newJog1 newJog2 newJogVez
    return ()

