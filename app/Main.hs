-- Declaração dos modulos
module Main where

import System.IO

-- Importes
import Util
import Tipos
import Peca
import Tabuleiro

import Ludo

ajuda :: IO()
ajuda = do
    cls
    putStrLn "\n------------------------------------------------------------------------- Como Jogar ------------------------------------------------------------------------\n"
    putStrLn "Para se iniciar a partida, joga-se o dado e o participante que fizer o maior \n\ 
    \número de pontos (6) inicia o jogo, continuando as jogadas em sentido horário. \n\
    \Joga-se com um dado e os avanços são feitos de acordo com os pontos obtidos com o lançamento \n\
    \dos dados. Cada jogador lança o dado e se tirar (6) poderá sair da casa de partida, sendo que, ao \n\
    \tirar o mesmo, você tem o direito de jogar o dado novamente.\n\n\ 
    \Pode ser jogado por um player e um bot baseado em heurística e aleatoriedade. \n\
    \O tabuleiro quadrado tem um percurso em forma de cruz e cada jogador tem quatro peões. \n\
    \Um dado define os movimentos. Os peões de cada jogador começam na base de mesma cor. \n\
    \O objetivo do jogo é ser o primeiro a levar seus 4 peões a dar uma volta no tabuleiro \n\
    \e a chegar no ponto final marcado com sua cor. Os peões movem-se pelo percurso no sentido horário. \n\
    \Para transportar um peão de sua base para seu ponto de partida é necessário tirar 6. \n\n\
    \Quando o jogador já tem pelo menos um peão no percurso, ele pode mover o peão do número de \n\
    \casas tirado no dado. Se tirar 6, além de usar esse resultado ele pode jogar novamente o dado. \n\
    \Se um jogador chegar a uma casa já ocupada por um peão adversario, o peão adversário \n\
    \deve voltar para sua base. Mas se 2 peões da mesma cor ocuparem uma mesma casa, eles não podem \n\
    \ser capturados e nenhum adversário pode passar por essa casa, tendo seus peões bloqueados. \n\
    \Após dar a volta no tabuleiro o peão avança pela reta final, de sua própria cor. \n\
    \A chegada ao ponto final só pode ser obtida por um número exato nos dados. \n\
    \Se o jogador tirar mais do que o necessário, ele vai ate o fim e volta, tendo que aguardar \n\
    \sua próxima jogada.\n\n\ 
    \O vencedor é o primeiro a levar seus quatro peões ao ponto de chegada da sua cor.\n"
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
        putStrLn $ setColorRed "\nOpção inválida, Pressione <Enter> para voltar\n"  
        getChar -- descarta o enter
        main

main :: IO ()
main = do
    --hSetBuffering stdin NoBuffering

    cls
    putStrLn $ setColorRed ludoLogo
    putStrLn $ setColorCiano "(1)" ++ " Jogar"
    putStrLn $ setColorCiano "(2)" ++ " Ajuda"
    putStrLn $ setColorCiano "(3)" ++ " Creditos"
    putStrLn $ setColorCiano "(4)" ++ " Sair"
    putStrLn $ setColorGreen "-----\nOpção: "
    op <- getLine
    executaOpcaoMain op
    
    return ()


