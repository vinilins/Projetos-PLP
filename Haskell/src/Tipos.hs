{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DeriveAnyClass #-}

module Tipos where

import qualified Data.Map as Map
import Data.Aeson
import GHC.Generics

-- Cores de uma peça ou jogador
data Cor =  Amarelo | Vermelho | Verde | Azul deriving (Eq, Ord, Show, Read, Bounded, Enum, Generic, ToJSON, FromJSON)

-- Movimentos de uma peça
data Movimento = Cima | Baixo | Esquerda | Direita | CimaEsquerda | CimaDireita | BaixoEsquerda | BaixoDireita deriving (Eq, Ord, Show, Read, Bounded, Enum, Generic, ToJSON, FromJSON)

-- Representação de uma posição
type Linha = Int
type Coluna = Int
type Posicao = (Linha, Coluna)

-- Representação do número de um dado
type NumDado = Int

-- Representação do número de movimentos de volta de uma peça
type NumVoltas = Int

-- Representação de uma opção
type Opcao = String

-- Representação de um jogador
data Jogador = Jogador {
    corJogador :: Cor,
    bot :: Bool
} deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON)

-- Representação de uma peça
data Peca = Peca {
    corPeca :: Cor,
    nomePeca :: String,
    listaMovimentosVitoria :: [Movimento]
} deriving (Ord, Read, Generic, ToJSON, FromJSON)

-- Definindo condição de igualdade e diferença entre pecas
instance Eq Peca  where
    (==), (/=) :: Peca -> Peca -> Bool
    p1 /= p2 = nomePeca p1 /= nomePeca p2
    p1 == p2 = nomePeca p1 == nomePeca p2
   
-- Representação de uma casa de tabuleiro
type CasaTabuleiro = [Peca]

-- Representação de um tabuleiro
type Tabuleiro = Map.Map Posicao CasaTabuleiro
