module Util where

import System.Process

import Tipos

getResetColor :: String
getResetColor = "\ESC[39m"

setColorWhite :: String -> String
setColorWhite str = "\ESC[37m" ++ str ++ getResetColor

setColorRed :: String -> String
setColorRed str = "\ESC[31m" ++ str ++ getResetColor

setColorGreen :: String -> String
setColorGreen str = "\ESC[32m" ++ str ++ getResetColor

setColorYellow :: String -> String
setColorYellow str = "\ESC[33m" ++ str ++ getResetColor

setColorMagenta :: String -> String
setColorMagenta str = "\ESC[35m" ++ str ++ getResetColor

setColorCiano :: String -> String
setColorCiano str = "\ESC[36m" ++ str ++ getResetColor -- normal 36 azul = 34

setColor :: String -> Cor -> String
setColor str c 
    | c == Amarelo = setColorYellow str
    | c == Vermelho = setColorRed str
    | c == Verde = setColorGreen str
    | c == Azul = setColorCiano str
    | otherwise = setColorWhite str

toStringPecaComCor :: Peca -> String
toStringPecaComCor peca = setColor (nomePeca peca) (corPeca peca)

toStringJogadorComCor :: Jogador -> String
toStringJogadorComCor jog 
    | bot jog = setColor (show(corJogador jog)) (corJogador jog) ++ setColorCiano " (BOT)"
    | otherwise = setColor (show(corJogador jog)) (corJogador jog)

toStringOpcao :: String
toStringOpcao = setColorGreen "-----\nOpção: "

toStringOpcaoInvalida :: String
toStringOpcaoInvalida = setColorRed "\nOpção inválida, Pressione <Enter> para voltar\n" 

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
