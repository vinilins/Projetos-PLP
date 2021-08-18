module Tipos where

import Array

data Cor =  Amarelo | Vermelho | Verde | Azul deriving (Eq, Ord, Show, Read, Bounded, Enum)

type Posicao = (Int, Int) -- (Linha, Coluna)

type CasaTabuleiro = {
    posicao :: Posicao,
    valida :: Boll,
    pecas :: [Peca]
} deriving (Eq, Ord, Show, Read)

type Jogador = {
    cor :: Cor
} deriving (Eq, Ord, Show, Read)

type Peca = {
    cor :: Cor
} deriving (Eq, Ord, Show, Read)

type Tabuleiro = [CasaTabuleiro] -- 196 casaTabuleiro