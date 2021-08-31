module Util where

import System.Process

printPecaComCor :: Peca -> String
printPecaComCor peca = setColor (nomePeca peca) (corPeca peca)

printJogadorComCor :: Jogador -> String
printJogadorComCor jog 
    | bot jog = setColor (show(corJogador jog)) (corJogador jog) ++ setColorCiano " (BOT)"
    | otherwise = setColor (show(corJogador jog)) (corJogador jog)

jogadorNormal :: Bool
jogadorNormal = False 

jogadorBot :: Bool
jogadorBot = True

-- Limpa o terminal
cls = system "cls"

-- String com a logo do jogo
ludoLogo :: String
ludoLogo = "|----------------------------------------------------------------|\n"++
           "|               ██╗     ██╗   ██╗██████╗  ██████╗                |\n"++
           "|               ██║     ██║   ██║██╔══██╗██╔═══██╗               |\n"++
           "|               ██║     ██║   ██║██║  ██║██║   ██║               |\n"++
           "|               ██║     ██║   ██║██║  ██║██║   ██║               |\n"++
           "|               ███████╗╚██████╔╝██████╔╝╚██████╔╝               |\n"++
           "|               ╚══════╝ ╚═════╝ ╚═════╝  ╚═════╝                |\n"++
           "|----------------------------------------------------------------|\n"
