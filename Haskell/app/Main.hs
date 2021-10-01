-- Declaração dos modulos
module Main where

import System.IO

import Util
import Tipos
import Peca
import Tabuleiro
import Ludo

ajuda :: IO()
ajuda = do
    cls
    putStrLn "\n------------------------------------------------------------------------- Como Jogar ------------------------------------------------------------------------\n"
    putStrLn "Objetivo do jogo:\n\n\
    \   * Percorrer todo o trajeto do tabuleiro no sentido horario com todas as peças e ser o primeiro jogador\n\ 
    \     a levar seus quatro peões ao ponto de chegada da sua cor.\n\n\
    \Regras:\n\n\
    \   * Pode ser jogado por um player e um bot baseado em heurística e aleatoriedade.\n\n\
    \   * O tabuleiro quadrado tem um percurso em forma de cruz e cada jogador tem quatro peões.\n\n\
    \   * Os peões de cada jogador começam na base de mesma cor.\n\n\
    \   * Para se iniciar a partida, joga-se o dado e o participante que fizer o maior número de pontos (6)\n\ 
    \     inicia o jogo retirando uma peça da base\n\n\
    \   * Quando o jogador já tem pelo menos um peão no percurso, joga-se um dado e os avanços são feitos \n\
    \     de acordo com os pontos obtidos com o lançamento do dado. Se tirar 6, além de usar esse resultado \n\
    \     ele pode jogar novamente o dado.\n\n\
    \   * Para transportar um peão de sua base para seu ponto de partida é necessário tirar (6).\n\n\
    \   * Se um jogador chegar a uma casa já ocupada por um peão adversário, o peão adversário deve voltar\n\ 
    \     para sua base.\n\n\
    \   * Mas se 2 peões da mesma cor ocuparem uma mesma casa, eles não podem ser capturados e nenhum adversário \n\ 
    \     pode passar por essa casa, tendo seus peões bloqueados.\n\n\
    \   * Após dar a volta no tabuleiro o peão avança pela reta final, de sua própria cor. A chegada ao ponto \n\ 
    \     final só pode ser obtida por um número exato nos dados. Se o jogador tirar mais do que o necessário,\n\ 
    \     ele vai até o fim e volta, tendo que aguardar sua próxima jogada.\n\n\
    \Outras Funcionalidades:\n\n\
    \   * O jogo terá um sistema de salvamento;\n\n\
    \   * Terá obstáculos pelo caminho como uma casa que retrocede duas casas;\n"

    putStrLn $ setColorGreen "Pressione <Enter> para voltar\n"
    getChar -- descarta o enter
    main

creditos :: IO()
creditos = do
    cls
    putStrLn "\n------------------------------------------------------------------------- Desenvolvedores ------------------------------------------------------------------------\n"
    putStrLn $ setColorCiano "Felipe Oliveira"
    putStrLn $ setColorCiano "Lucas Lima"
    putStrLn $ setColorCiano "Pedro Manoel"
    putStrLn $ setColorCiano "Vinícius Lins"
    putStrLn $ setColorGreen "\nPressione <Enter> para voltar\n"
    getChar -- descarta o enter
    main

sair :: IO()
sair = do
    putStrLn $ setColorCiano "\nObrigado por jogar\n"

executaOpcaoMain :: Opcao -> IO()
executaOpcaoMain op
    | op == "1" = do 
        iniciarMenuLudo
        main
    | op == "2" = ajuda
    | op == "3" = creditos
    | op == "4" = sair
    | otherwise = do 
        putStrLn toStringOpcaoInvalida   
        getChar -- descarta o enter
        main

main :: IO ()
main = do
    cls
    putStrLn $ setColorRed ludoLogo
    putStrLn $ setColorCiano "(1)" ++ " Jogar"
    putStrLn $ setColorCiano "(2)" ++ " Ajuda"
    putStrLn $ setColorCiano "(3)" ++ " Creditos"
    putStrLn $ setColorCiano "(4)" ++ " Sair"
    putStrLn toStringOpcao
    op <- getLine
    executaOpcaoMain op
    
    return ()
