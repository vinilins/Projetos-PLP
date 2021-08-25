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
    \numero de pontos (6) inicia o jogo, continuando as jogadas em sentido horario. \n\
    \Joga-se com um dado e os avanços sao feitos de acordo com os pontos obtidos com o lancamento \n\
    \dos dados. Cada jogador lanca o dado e se tirar (6) podera sair da casa de partida, sendo que, ao \n\
    \tirar o mesmo, voce tem o direito de jogar o dado novamente.\n\n\ 
    \Pode ser jogado por um player e um bot baseado em heuristica e aleatoriedade. \n\
    \O tabuleiro quadrado tem um percurso em forma de cruz e cada jogador tem quatro peoes. \n\
    \Um dado define os movimentos. Os peoes de cada jogador comecam na base de mesma cor. \n\
    \O objetivo do jogo eh ser o primeiro a levar seus 4 peoes a dar uma volta no tabuleiro \n\
    \e a chegar no ponto final marcado com sua cor. Os peoes movem-se pelo percurso no sentido horario. \n\
    \Para transportar um peao de sua base para seu ponto de partida eh necessario tirar 6. \n\n\
    \Quando o jogador ja tem pelo menos um peao no percurso, ele pode mover o peao do numero de \n\
    \casas tirado no dado. Se tirar 6, alem de usar esse resultado ele pode jogar novamente o dado. \n\
    \Se um jogador chegar a uma casa ja ocupada por um peao adversario, o peao adversario \n\
    \deve voltar para sua base. Mas se 2 peoes da mesma cor ocuparem uma mesma casa, eles nao podem \n\
    \ser capturados e nenhum adversario pode passar por essa casa, tendo seus peoes bloqueados. \n\
    \Apos dar a volta no tabuleiro o peao avanca pela reta final, de sua propria cor. \n\
    \A chegada ao ponto final so pode ser obtida por um numero exato nos dados. \n\
    \Se o jogador tirar mais do que o necessario, ele vai ate o fim e volta, tendo que aguardar \n\
    \sua proxima jogada.\n\n\ 
    \O vencedor eh o primeiro a levar seus quatro peoes ao ponto de chegada da sua cor.\n"
    putStrLn "Pressione <Enter> para voltar\n" 
    getChar -- descarta o enter
    main

creditos :: IO()
creditos = do
    cls
    putStrLn "\n-------------------------------- Desenvolvedores --------------------------------"
    putStrLn "Emanuel Moura, Felipe Oliveira, Lucas Lima, Pedro Manoel, Vinicius Lins\n"
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

main :: IO ()
main = do
    cls
    putStrLn "-------------------------------- Jogo do LUDO --------------------------------"
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


