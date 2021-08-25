module Util where

-- Importes
import System.Process

-- remove um elemento de uma lista
remove :: Eq a => a -> [a] -> [a]
remove element = filter (/= element)

-- adiciona um elemento em uma lista
insert :: a -> [a] -> [a]
insert element list = element : list

-- Limpa o terminal
cls = system "cls"