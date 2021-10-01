:- module(bot, [
    decideJogadaBot/4
]).

:- use_module('src/tabuleiro.pl').
:- use_module('src/posicao.pl').
:- use_module('src/util.pl').

getMaxAva([Ava], Ava) :- !, true.
getMaxAva([Ava|T], Max):- getMaxAva(T, Max), Max.pontos >= Ava.pontos.
getMaxAva([Ava|T], Ava):- getMaxAva(T, Max), Ava.pontos > Max.pontos.

% Avaliando se a peça chegou na base final
avaliaPecaNaBaseFinal(Peca, 25) :-
    posiBaseFinal(Peca.cor, Peca.posi), !.

avaliaPecaNaBaseFinal(_, 0).

% Avaliando se a peca saiu da base inicial
avaliaPecaSaiuDaBaseInicial(Peca, NewPeca, 45) :-
    posiBaseInicial(Peca.cor, Peca.posi),
    not(posiBaseInicial(NewPeca.cor, NewPeca.posi)), !.

avaliaPecaSaiuDaBaseInicial(_, _, 0).

% Avaliando se a peça não se moveu
avaliaPecaNaoSeMoveu(Peca, NewPeca, -25) :-
    Peca.posi == NewPeca.posi, !.

avaliaPecaNaoSeMoveu(_, _, 0).

% Avaliando se a peça comeu uma peça adversária
avaliaPecaComeuPecaAdversaria(Peca, Tab, NewTab, 60) :-
    corOposta(Peca.cor, CorAdv),
    numPecasBaseInicial(CorAdv, Tab, NumTab),
    numPecasBaseInicial(CorAdv, NewTab, NumNewTab),
    NumNewTab > NumTab, !.

avaliaPecaComeuPecaAdversaria(_, _, _, 0).

% Atribuinda a pontuação da peça
pontosJogarPeca(Peca, Tab, NewPeca, NewTab, Pontos) :-
    avaliaPecaNaBaseFinal(NewPeca, P1),
    avaliaPecaSaiuDaBaseInicial(Peca, NewPeca, P2),
    avaliaPecaNaoSeMoveu(Peca, NewPeca, P3),
    avaliaPecaComeuPecaAdversaria(Peca, Tab, NewTab, P4),
    Pontos is P1 + P2 + P3 + P4.

% Atribuinda a pontuação de cada peça
avaliaJogarPecas(_, _, [], []). 
avaliaJogarPecas(Dado, Tab, [Peca|T], [ava{peca: Peca, pontos: Pontos}|T2]) :-
    executaMovimentosPeca(Peca, Dado, Tab, NewTab),
    getPecaTab(Peca, NewTab, NewPeca),
    pontosJogarPeca(Peca, Tab, NewPeca, NewTab, Pontos),
    avaliaJogarPecas(Dado, Tab, T, T2).

% quando só existe uma peça na lista de peças jogaveis 
decideJogadaBot(_, _, [Peca|T], Peca) :-
    length([Peca|T], LenPecas),
    LenPecas == 1.

% quando existe mais de uma peça na lista de peças jogaveis
decideJogadaBot(Ludo, Dado, Pecas, Peca) :-
    length(Pecas, LenPecas),
    LenPecas > 1,
    avaliaJogarPecas(Dado, Ludo.tab, Pecas, ListAva), % gerando a lista com as avaliações das jogadas
    getMaxAva(ListAva, MaxAva), % jogada com a avaliação de melhor pontuação
    include(filterDictKeyValue(pontos, MaxAva.pontos), ListAva, NewListAva), % lista com as jogadas de melhor pontuação
    random_permutation(NewListAva, NewSortListAva), % lista com as jogadas de melhor pontuação ordenada de forma aleatória 
    nth1(1, NewSortListAva, MaxSortAva), % Escolhendo uma das jogadas de melhor pontuação
    Peca = MaxSortAva.peca, % Escolhendo a peca da jogada escolhida

    % debug
    maplist(getDictKey(peca), ListAva, ListPecas),
    maplist(getDictKey(nome), ListPecas, ListNomesPecas),
    maplist(getDictKey(pontos), ListAva, ListPontosPecas),
    writeln(ListNomesPecas),
    writeln(ListPontosPecas),
    writeln(Peca.nome).
 