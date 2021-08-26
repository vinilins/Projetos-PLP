-- Declaração dos modulos
module Main where

-- Importes
import Util
import Ludo
import Tipos
import Peca
import Tabuleiro

import Arquivo

ajuda :: IO()
ajuda = do
    cls
    putStrLn "\n-------------------------------- Como Jogar -------------------------------------"
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
    putStrLn "Pressione <Enter> para voltar\n" 
    getChar -- descarta o enter
    main

creditos :: IO()
creditos = do
    cls
    putStrLn "\n-------------------------------- Desenvolvedores --------------------------------"
    putStrLn "Felipe Oliveira, Lucas Lima, Pedro Manoel, Vinícius Lins\n"
    putStrLn "Pressione <Enter> para voltar\n" 
    getChar -- descarta o enter
    main

sair :: IO()
sair = do
    putStrLn "\nObrigado por jogar"

novoJogo :: IO()
novoJogo = do
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

    runLudo tabuleiro jogador1 jogador2 jogador1
    main

continuar :: IO()
continuar = do
    putStrLn "\nEM CONSTRUÇÃO"
    putStrLn "Pressione <Enter> para voltar\n" 
    getChar -- descarta o enter
    mainArquivo

executaOpcaoMain :: Char -> IO()
executaOpcaoMain op
    | op == '1' = novoJogo
    | op == '2' = continuar
    | op == '3' = ajuda
    | op == '4' = creditos
    | op == '5' = sair
    | otherwise = do 
        putStrLn "\nOpção inválida, Pressione <Enter> para voltar\n"  
        getChar -- descarta o enter
        main

ludoLogo :: String
ludoLogo = "|----------------------------------------------------------------|\n" ++
           "|               ██╗     ██╗   ██╗██████╗  ██████╗                |\n"++
           "|               ██║     ██║   ██║██╔══██╗██╔═══██╗               |\n"++
           "|               ██║     ██║   ██║██║  ██║██║   ██║               |\n"++
           "|               ██║     ██║   ██║██║  ██║██║   ██║               |\n"++
           "|               ███████╗╚██████╔╝██████╔╝╚██████╔╝               |\n"++
           "|               ╚══════╝ ╚═════╝ ╚═════╝  ╚═════╝                |\n"++
           "|----------------------------------------------------------------|\n"


main :: IO ()
main = do
    cls
    putStrLn ludoLogo
    putStrLn "(1) Novo Jogo"
    putStrLn "(2) Continuar"
    putStrLn "(3) Ajuda"
    putStrLn "(4) Creditos"
    putStrLn "(5) Sair"
    putStrLn "-----\nOpção: "
    op <- getChar
    getChar -- descarta o Enter
    executaOpcaoMain op
    
    return ()


