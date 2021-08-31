module Util where

import System.Process
import Tipos

getReset :: String
getReset = "\ESC[39m"

setColorWhite :: String -> String
setColorWhite str = "\ESC[37m" ++ str ++ getReset

setColorBlack :: String -> String
setColorBlack str = "\ESC[30m" ++ str ++ getReset

setColorRed :: String -> String
setColorRed str = "\ESC[31m" ++ str ++ getReset

setColorGreen :: String -> String
setColorGreen str = "\ESC[32m" ++ str ++ getReset

setColorYellow :: String -> String
setColorYellow str = "\ESC[33m" ++ str ++ getReset

setColorBlue :: String -> String
setColorBlue str = "\ESC[34m" ++ str ++ getReset

setColorMagenta :: String -> String
setColorMagenta str = "\ESC[35m" ++ str ++ getReset

setColorCiano :: String -> String
setColorCiano str = "\ESC[36m" ++ str ++ getReset

setColor :: String -> Cor -> String
setColor str c 
    | c == Amarelo = setColorYellow str
    | c == Vermelho = setColorRed str
    | c == Verde = setColorGreen str
    | c == Azul = setColorBlue str
    | otherwise = setColorWhite str

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
