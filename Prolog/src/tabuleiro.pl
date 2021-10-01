:- module(tabuleiro, [
    criaTabComPecas/3,
    numPecasBaseFinal/3,
    numPecasBaseInicial/3,
    printTab/1,
    executaMovimentosPeca/4,
    getCasaTab/3,
    listCasas/3,
    getPecaTab/3
]).

:- use_module('src/util.pl').
:- use_module('src/peca.pl').
:- use_module('src/posicao.pl').

criaTabVazio(Tab) :-
    Tab = tabuleiro{
        l1:_{c1:[], c2:[], c3:[], c4:[], c5:[], c6:[], c7:[], c8:[], c9:[], c10:[], c11:[], c12:[], c13:[], c14:[], c15:[]},
        l2:_{c1:[], c2:[], c3:[], c4:[], c5:[], c6:[], c7:[], c8:[], c9:[], c10:[], c11:[], c12:[], c13:[], c14:[], c15:[]},
        l3:_{c1:[], c2:[], c3:[], c4:[], c5:[], c6:[], c7:[], c8:[], c9:[], c10:[], c11:[], c12:[], c13:[], c14:[], c15:[]},
        l4:_{c1:[], c2:[], c3:[], c4:[], c5:[], c6:[], c7:[], c8:[], c9:[], c10:[], c11:[], c12:[], c13:[], c14:[], c15:[]},
        l5:_{c1:[], c2:[], c3:[], c4:[], c5:[], c6:[], c7:[], c8:[], c9:[], c10:[], c11:[], c12:[], c13:[], c14:[], c15:[]},
        l6:_{c1:[], c2:[], c3:[], c4:[], c5:[], c6:[], c7:[], c8:[], c9:[], c10:[], c11:[], c12:[], c13:[], c14:[], c15:[]},
        l7:_{c1:[], c2:[], c3:[], c4:[], c5:[], c6:[], c7:[], c8:[], c9:[], c10:[], c11:[], c12:[], c13:[], c14:[], c15:[]},
        l8:_{c1:[], c2:[], c3:[], c4:[], c5:[], c6:[], c7:[], c8:[], c9:[], c10:[], c11:[], c12:[], c13:[], c14:[], c15:[]},
        l9:_{c1:[], c2:[], c3:[], c4:[], c5:[], c6:[], c7:[], c8:[], c9:[], c10:[], c11:[], c12:[], c13:[], c14:[], c15:[]},
        l10:_{c1:[], c2:[], c3:[], c4:[], c5:[], c6:[], c7:[], c8:[], c9:[], c10:[], c11:[], c12:[], c13:[], c14:[], c15:[]},
        l11:_{c1:[], c2:[], c3:[], c4:[], c5:[], c6:[], c7:[], c8:[], c9:[], c10:[], c11:[], c12:[], c13:[], c14:[], c15:[]},
        l12:_{c1:[], c2:[], c3:[], c4:[], c5:[], c6:[], c7:[], c8:[], c9:[], c10:[], c11:[], c12:[], c13:[], c14:[], c15:[]},
        l13:_{c1:[], c2:[], c3:[], c4:[], c5:[], c6:[], c7:[], c8:[], c9:[], c10:[], c11:[], c12:[], c13:[], c14:[], c15:[]},
        l14:_{c1:[], c2:[], c3:[], c4:[], c5:[], c6:[], c7:[], c8:[], c9:[], c10:[], c11:[], c12:[], c13:[], c14:[], c15:[]},
        l15:_{c1:[], c2:[], c3:[], c4:[], c5:[], c6:[], c7:[], c8:[], c9:[], c10:[], c11:[], c12:[], c13:[], c14:[], c15:[]}
    }.

getCasaTab(Posi, Tab, Casa) :-
    atom_concat(l, Posi.lin, Lin),
    atom_concat(c, Posi.col, Col),
    Casa = Tab.get(Lin).get(Col).

putCasaTab(Posi, Tab, Casa, NewTab) :-
    atom_concat(l, Posi.lin, Lin),
    atom_concat(c, Posi.col, Col),
    NewTab = Tab.put(Lin/Col, Casa).

printPecasCasaTab(Posi, Tab) :-
    getCasaTab(Posi, Tab, Casa),
    printPecas(Casa).
    
printColunasLinhaTab([], _).
printColunasLinhaTab([Posi|T], Tab) :- 
    posiVoltaDuasCasas(Posi), 
    printColorMagenta('[   -2   ]'), 
    printColunasLinhaTab(T, Tab).
printColunasLinhaTab([Posi|T], Tab) :- 
    isPosiMovi(Posi), 
    numPecasCasaTab(Posi, Tab, Num), 
    Num =:= 0,
    posiTerritorioCor(Posi, Cor), 
    printColor(Cor, '[        ]'), 
    printColunasLinhaTab(T, Tab).
printColunasLinhaTab([Posi|T], Tab) :- 
    isPosiMovi(Posi), 
    numPecasCasaTab(Posi, Tab, Num), 
    Num =:= 1,
    posiTerritorioCor(Posi, Cor), 
    printColor(Cor, '[   '), printPecasCasaTab(Posi, Tab), printColor(Cor, '   ]'),
    printColunasLinhaTab(T, Tab).
printColunasLinhaTab([Posi|T], Tab) :- 
    isPosiMovi(Posi), 
    numPecasCasaTab(Posi, Tab, Num), 
    Num =:= 2,
    posiTerritorioCor(Posi, Cor), 
    printColor(Cor, '[  '), printPecasCasaTab(Posi, Tab), printColor(Cor, '  ]'),
    printColunasLinhaTab(T, Tab).
printColunasLinhaTab([Posi|T], Tab) :- 
    isPosiMovi(Posi), 
    numPecasCasaTab(Posi, Tab, Num), 
    Num =:= 3,
    posiTerritorioCor(Posi, Cor), 
    printColor(Cor, '[ '), printPecasCasaTab(Posi, Tab), printColor(Cor, ' ]'), 
    printColunasLinhaTab(T, Tab).
printColunasLinhaTab([Posi|T], Tab) :- 
    isPosiMovi(Posi), 
    numPecasCasaTab(Posi, Tab, Num), 
    Num =:= 4, 
    posiTerritorioCor(Posi, Cor), 
    printColor(Cor, '['), printPecasCasaTab(Posi, Tab), printColor(Cor, ']'), 
    printColunasLinhaTab(T, Tab).
printColunasLinhaTab([_|T], Tab) :- 
    write('          '), 
    printColunasLinhaTab(T, Tab).

printLinhasTab([], _).
printLinhasTab([Posis|T], Tab) :- printColunasLinhaTab(Posis, Tab), nl, printLinhasTab(T, Tab).

printTab(Tab) :-
    matrizPosiTab(MT),
    printLinhasTab(MT, Tab).

adicionaPecasCasaBaseTab(Pecas, Cor, Tab, NewTab) :- 
    posiBaseInicial(Cor, Posi),
    getCasaTab(Posi, Tab, Casa),
    append(Casa, Pecas, NewCasa),
    putCasaTab(Posi, Tab, NewCasa, NewTab).

criaTabComPecas(Cor1, Cor2, NewTab) :-
    criaTabVazio(Tab),
    listPecasCor(Cor1, Pecas1),
    listPecasCor(Cor2, Pecas2),
    adicionaPecasCasaBaseTab(Pecas1, Cor1, Tab, Tab1),
    adicionaPecasCasaBaseTab(Pecas2, Cor2, Tab1, NewTab).
 
getCasaBaseInicialTab(Cor, Tab, Casa) :- 
    posiBaseInicial(Cor, Posi),
    getCasaTab(Posi, Tab, Casa).

getCasaBaseFinalTab(Cor, Tab, Casa) :- 
    posiBaseFinal(Cor, Posi),
    getCasaTab(Posi, Tab, Casa).

numPecasBaseFinal(Cor, Tab, Num) :- 
    getCasaBaseFinalTab(Cor, Tab, Casa),
    length(Casa, Num).

numPecasBaseInicial(Cor, Tab, Num) :- 
    getCasaBaseInicialTab(Cor, Tab, Casa),
    length(Casa, Num).

numPecasCasaTab(Posi, Tab, Num) :- 
    getCasaTab(Posi, Tab, Casa),
    length(Casa, Num).
    
listPosiBaseInicial(LP) :-
    findall(P, posiBaseInicial(_, P), LP).

listPosiBaseFinal(LP) :-
    findall(P, posiBaseFinal(_, P), LP).

adicionaPecaCasaTab(Peca, Posi, Tab, NewTab) :- 
    getCasaTab(Posi, Tab, Casa),
    insertHeadList(Peca, Casa, NewCasa),
    putCasaTab(Posi, Tab, NewCasa, NewTab).

removePecaCasaTab(Peca, Posi, Tab, NewTab) :-
    getCasaTab(Posi, Tab, Casa),
    removePeca(Peca, Casa, NewCasa),
    putCasaTab(Posi, Tab, NewCasa, NewTab).

getPecaTab(Peca, Tab, NewPeca) :-
    listPosiMovi(PL),
    listCasas(PL, Tab, PL1),
    append(PL1, PL2),
    include(filterDictKeyValue(nome, Peca.nome), PL2, R),
    nth1(1, R, NewPeca).
    
movePecaTab(Peca, Posi, Tab, NewTab, NewPeca) :-
    NewPeca = Peca.put(posi, Posi),
    removePecaCasaTab(Peca, Peca.posi, Tab, Tab1),
    adicionaPecaCasaTab(NewPeca, Posi, Tab1, NewTab).

movePecaCimaTab(Peca, Tab, NewTab, NewPeca) :-
    Lin is Peca.posi.lin - 1,
    Col = Peca.posi.col,
    criaPosi(Lin, Col, NewPosi),
    movePecaTab(Peca, NewPosi, Tab, NewTab, NewPeca).

movePecaBaixoTab(Peca, Tab, NewTab, NewPeca) :-
    Lin is Peca.posi.lin + 1,
    Col = Peca.posi.col,
    criaPosi(Lin, Col, NewPosi),
    movePecaTab(Peca, NewPosi, Tab, NewTab, NewPeca).

movePecaEsquerdaTab(Peca, Tab, NewTab, NewPeca) :-
    Lin = Peca.posi.lin,
    Col is Peca.posi.col - 1,
    criaPosi(Lin, Col, NewPosi),
    movePecaTab(Peca, NewPosi, Tab, NewTab, NewPeca).

movePecaDireitaTab(Peca, Tab, NewTab, NewPeca) :-
    Lin = Peca.posi.lin,
    Col is Peca.posi.col + 1,
    criaPosi(Lin, Col, NewPosi),
    movePecaTab(Peca, NewPosi, Tab, NewTab, NewPeca).

movePecaCimaEsquerdaTab(Peca, Tab, NewTab, NewPeca) :-
    Lin is Peca.posi.lin - 1,
    Col is Peca.posi.col - 1,
    criaPosi(Lin, Col, NewPosi),
    movePecaTab(Peca, NewPosi, Tab, NewTab, NewPeca).

movePecaCimaDireitaTab(Peca, Tab, NewTab, NewPeca) :-
    Lin is Peca.posi.lin - 1,
    Col is Peca.posi.col + 1,
    criaPosi(Lin, Col, NewPosi),
    movePecaTab(Peca, NewPosi, Tab, NewTab, NewPeca).

movePecaBaixoDireitaTab(Peca, Tab, NewTab, NewPeca) :-
    Lin is Peca.posi.lin + 1,
    Col is Peca.posi.col + 1,
    criaPosi(Lin, Col, NewPosi),
    movePecaTab(Peca, NewPosi, Tab, NewTab, NewPeca).

movePecaBaixoEsquerdaTab(Peca, Tab, NewTab, NewPeca) :-
    Lin is Peca.posi.lin + 1,
    Col is Peca.posi.col - 1,
    criaPosi(Lin, Col, NewPosi),
    movePecaTab(Peca, NewPosi, Tab, NewTab, NewPeca).

executaMoviPeca(Peca, Tab, cima, NewTab, NewPeca) :- movePecaCimaTab(Peca, Tab, NewTab, NewPeca).
executaMoviPeca(Peca, Tab, baixo, NewTab, NewPeca) :- movePecaBaixoTab(Peca, Tab, NewTab, NewPeca).
executaMoviPeca(Peca, Tab, direita, NewTab, NewPeca) :- movePecaDireitaTab(Peca, Tab, NewTab, NewPeca).
executaMoviPeca(Peca, Tab, esquerda, NewTab, NewPeca) :- movePecaEsquerdaTab(Peca, Tab, NewTab, NewPeca).
executaMoviPeca(Peca, Tab, cimaEsquerda, NewTab, NewPeca) :- movePecaCimaEsquerdaTab(Peca, Tab, NewTab, NewPeca).
executaMoviPeca(Peca, Tab, baixoDireita, NewTab, NewPeca) :- movePecaBaixoDireitaTab(Peca, Tab, NewTab, NewPeca).
executaMoviPeca(Peca, Tab, cimaDireita, NewTab, NewPeca) :- movePecaCimaDireitaTab(Peca, Tab, NewTab, NewPeca).
executaMoviPeca(Peca, Tab, baixoEsquerda, NewTab, NewPeca) :- movePecaBaixoEsquerdaTab(Peca, Tab, NewTab, NewPeca).

getPecasOutraCorNaCasa(Peca, Tab, PecasOutraCor) :-
    getCasaTab(Peca.posi, Tab, Casa),
    include(filterDictNotKeyValue(cor, Peca.cor), Casa, PecasOutraCor).

% inicio da execução dos movimentos de uma peça
executaMovimentosPeca(Peca, Dado, Tab, NewTab) :-
    executaMovimentosFrentePeca(Peca, Dado, Tab, NewTab).

% movendo a peça para frente

% se a peça terminou numa casa volta duas, então ela se move duas vezes para trás
executaMovimentosFrentePeca(Peca, 0, Tab, R) :-
    posiVoltaDuasCasas(Peca.posi),
    executaMovimentosTrasPeca(Peca, 2, Tab, R).

% se a peça terminou numa posição onde existe somente outra peca de outra cor, essa peça de outra cor volta para a base dela
executaMovimentosFrentePeca(Peca, 0, Tab, R) :-
    getPecasOutraCorNaCasa(Peca, Tab, PecasOutraCor),
    length(PecasOutraCor, LenPecasOutraCor),
    LenPecasOutraCor == 1,
    nth1(1, PecasOutraCor, OutraPeca),
    criaPecaCor(OutraPeca.cor, OutraPeca.nome, OutraPeca1),
    removePecaCasaTab(OutraPeca, OutraPeca.posi, Tab, NewTab),
    adicionaPecaCasaTab(OutraPeca1, OutraPeca1.posi, NewTab, R).

% se uma posição a frente da peça existe duas ou mais peças de outra cor, a peca não se move pois está bloqueada
executaMovimentosFrentePeca(Peca, _, Tab, Tab) :-
    nth1(1, Peca.listMoviFre, Movi),
    executaMoviPeca(Peca, Tab, Movi, _, NewPeca),
    getPecasOutraCorNaCasa(NewPeca, Tab, PecasOutraCor),
    length(PecasOutraCor, LenPecasOutraCor),
    LenPecasOutraCor >= 2.

% quando a peça termina de se mover para frente
executaMovimentosFrentePeca(_, 0, Tab, Tab).

% o primeiro movimento da peça é o de tirar ela da base inicial
executaMovimentosFrentePeca(Peca, 6, Tab, NewTab) :-
    posiBaseInicial(Peca.cor, Peca.posi),
    executaMovimentosFrentePeca(Peca, 1, Tab, NewTab).

% se a peça chegou na base final, mas o valor do dado não é 0, ela precisa se mover para trás com o valor restante do dado
executaMovimentosFrentePeca(Peca, Dado, Tab, R) :-
    posiBaseFinal(Peca.cor, Peca.posi),
    Dado \== 0,
    executaMovimentosTrasPeca(Peca, Dado, Tab, R).

% executando um movimento para frente
executaMovimentosFrentePeca(Peca, Dado, Tab, R) :-
    nth1(1, Peca.listMoviFre, Movi, NewListMoviFre),
    movimentoInverso(Movi, MoviInv),
    insertHeadList(MoviInv, Peca.listMoviTra, NewListMoviTra),
    NewDado is Dado - 1,
    NewPeca = Peca.put(listMoviFre, NewListMoviFre),
    NewPeca1 = NewPeca.put(listMoviTra, NewListMoviTra),
    executaMoviPeca(NewPeca1, Tab, Movi, NewTab, NewPeca2),
    executaMovimentosFrentePeca(NewPeca2, NewDado, NewTab, R).

% movendo a peça para trás

% se a peça terminou numa posição onde existe somente outra peca de outra cor, essa peça de outra cor volta para a base dela
executaMovimentosTrasPeca(Peca, 0, Tab, R) :-
    getPecasOutraCorNaCasa(Peca, Tab, PecasOutraCor),
    length(PecasOutraCor, LenPecasOutraCor),
    LenPecasOutraCor == 1,
    nth1(1, PecasOutraCor, OutraPeca),
    criaPecaCor(OutraPeca.cor, OutraPeca.nome, OutraPeca1),
    removePecaCasaTab(OutraPeca, OutraPeca.posi, Tab, NewTab),
    adicionaPecaCasaTab(OutraPeca1, OutraPeca1.posi, NewTab, R).

% se uma posição a trás da peça existe duas ou mais peças de outra cor, a peca não se move pois está bloqueada
executaMovimentosTrasPeca(Peca, _, Tab, Tab) :-
    nth1(1, Peca.listMoviTra, Movi),
    executaMoviPeca(Peca, Tab, Movi, _, NewPeca),
    getPecasOutraCorNaCasa(NewPeca, Tab, PecasOutraCor),
    length(PecasOutraCor, LenPecasOutraCor),
    LenPecasOutraCor >= 2.

% quando a peça termina de se mover para trás
executaMovimentosTrasPeca(_, 0, Tab, Tab).

% executando um movimento para trás
executaMovimentosTrasPeca(Peca, Dado, Tab, R) :-
    nth1(1, Peca.listMoviTra, Movi, NewListMoviTra),
    movimentoInverso(Movi, MoviInv),
    insertHeadList(MoviInv, Peca.listMoviFre, NewListMoviFre),
    NewDado is Dado - 1,
    NewPeca = Peca.put(listMoviFre, NewListMoviFre),
    NewPeca1 = NewPeca.put(listMoviTra, NewListMoviTra),
    executaMoviPeca(NewPeca1, Tab, Movi, NewTab, NewPeca2),
    executaMovimentosTrasPeca(NewPeca2, NewDado, NewTab, R).
    
listCasas([], _, []).
listCasas([Posi|T], Tab, [Casa|T2]) :- getCasaTab(Posi, Tab, Casa), listCasas(T, Tab, T2).
 