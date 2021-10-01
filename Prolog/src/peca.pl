:- module(peca, [
    listPecasCor/2,
    movimentoInverso/2,
    criaPecaCor/3,
    removePeca/3,
    printPeca/1,
    printPecas/1
]).

:- use_module('posicao.pl').
:- use_module('util.pl').

movimento(cima).
movimento(baixo).
movimento(esquerda).
movimento(direita).
movimento(cimaEsquerda).
movimento(cimaDireita).
movimento(baixoEsquerda).
movimento(baixoDireita).

movimentoInverso(cima, baixo).
movimentoInverso(baixo, cima).
movimentoInverso(direita, esquerda).
movimentoInverso(esquerda, direita).
movimentoInverso(cimaEsquerda, baixoDireita).
movimentoInverso(baixoDireita, cimaEsquerda).
movimentoInverso(cimaDireita, baixoEsquerda).
movimentoInverso(baixoEsquerda, cimaDireita).

pecaMovimentosFrente(amarelo, [direita,cima,cima,cima,cima,cimaEsquerda,esquerda,esquerda,esquerda,esquerda,esquerda,cima,cima 
    ,direita,direita,direita,direita,direita,cimaDireita,cima,cima,cima,cima,cima,direita,direita,baixo,baixo,baixo,baixo,baixo,
    baixoDireita,direita,direita,direita,direita,direita,baixo,baixo,esquerda,esquerda,esquerda,esquerda,esquerda,baixoEsquerda,
    baixo,baixo,baixo,baixo,baixo,esquerda,cima,cima,cima,cima,cima,cima]).

pecaMovimentosFrente(vermelho, [baixo,direita,direita,direita,direita,cimaDireita, cima, cima, cima, cima, cima, direita, direita, 
    baixo, baixo, baixo, baixo, baixo, baixoDireita, direita, direita, direita, direita, direita, baixo, baixo, esquerda, esquerda, 
    esquerda, esquerda, esquerda, baixoEsquerda,  baixo, baixo, baixo, baixo, baixo, esquerda, esquerda, cima, cima, cima, cima, cima, 
    cimaEsquerda, esquerda, esquerda, esquerda, esquerda, esquerda, cima, direita, direita, direita, direita, direita, direita]).

pecaMovimentosFrente(verde, [esquerda,baixo,baixo,baixo,baixo,baixoDireita,direita,direita,direita,direita,direita,baixo,baixo,
    esquerda,esquerda,esquerda,esquerda,esquerda,baixoEsquerda,baixo,baixo,baixo,baixo,baixo,esquerda,esquerda, 
    cima,cima,cima,cima,cima,cimaEsquerda,esquerda,esquerda,esquerda,esquerda,esquerda,cima,cima, 
    direita,direita,direita,direita,direita,cimaDireita,cima,cima,cima,cima,cima,direita, 
    baixo,baixo,baixo,baixo,baixo,baixo]).

pecaMovimentosFrente(azul, [cima, esquerda, esquerda, esquerda, esquerda, baixoEsquerda, baixo, baixo, baixo, baixo, baixo, 
    esquerda, esquerda, cima, cima, cima, cima, cima, cimaEsquerda, esquerda, esquerda, esquerda, esquerda, esquerda, cima, 
    cima, direita, direita, direita, direita, direita, cimaDireita, cima, cima, cima, cima, cima, direita, direita, baixo, 
    baixo, baixo, baixo, baixo, baixoDireita, direita, direita, direita, direita, direita, baixo, esquerda, esquerda, esquerda, 
    esquerda, esquerda, esquerda]).

pecaNome(amarelo, 'A1').
pecaNome(amarelo, 'A2').
pecaNome(amarelo, 'A3').
pecaNome(amarelo, 'A4').

pecaNome(verde, 'V1').
pecaNome(verde, 'V2').
pecaNome(verde, 'V3').
pecaNome(verde, 'V4').

pecaNome(azul, 'A1').
pecaNome(azul, 'A2').
pecaNome(azul, 'A3').
pecaNome(azul, 'A4').

pecaNome(vermelho, 'V1').
pecaNome(vermelho, 'V2').
pecaNome(vermelho, 'V3').
pecaNome(vermelho, 'V4').

removePeca(Peca, [PecaL|T], T) :- Peca.nome == PecaL.nome.
removePeca(Peca, [PecaL|T], [PecaL|T1]) :- removePeca(Peca,T,T1).

criaPecaCor(Cor, Peca) :-
    pecaNome(Cor, Nome),
    pecaMovimentosFrente(Cor, ListMoviFrente),
    posiBaseInicial(Cor, Posi),
    Peca = peca{nome: Nome, cor: Cor, listMoviFre: ListMoviFrente, listMoviTra: [], posi:Posi}.

criaPecaCor(Cor, Nome, Peca) :-
    pecaMovimentosFrente(Cor, ListMoviFrente),
    posiBaseInicial(Cor, Posi),
    Peca = peca{nome: Nome, cor: Cor, listMoviFre: ListMoviFrente, listMoviTra: [], posi:Posi}.

listPecasCor(Cor, LP) :-
    findall(P, criaPecaCor(Cor, P), LP).

printPeca(Peca) :-
    printColor(Peca.cor, Peca.nome).

printPecas([]).
printPecas([Peca|T]) :- 
    printPeca(Peca), 
    printPecas(T).
