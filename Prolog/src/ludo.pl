:- module(ludo, [
    menuLudo/1    
]).

:- encoding(utf8).

:- use_module('util.pl').
:- use_module('posicao.pl').
:- use_module('bot.pl').
:- use_module('peca.pl').
:- use_module('tabuleiro.pl').

decideCorJogador(Cor) :-
    cls(),
    printLudoLogo(), nl,
    nl, writeln('Decida a sua cor:'), nl,
    printColorCyan('(1)'), printColorYellow(' Amarelo'), nl,
    printColorCyan('(2)'), printColorRed(' Vermelho'), nl,
    printColorCyan('(3)'), printColorGreen(' Verde'), nl,
    printColorCyan('(4)'), printColorCyan(' Azul'), nl,
    printOpcao(),
    readStr(Op),
    (
        atom_number(Op, OpN), cor(OpN, R) 
        -> 
            Cor = R
        ; 
            printOpcaoInvalida(),
            decideCorJogador(Cor)
    ).

% Bot x Bot
decideCorJogadores(true, true, C1, C2) :-
    random_between(1, 4, NumC1),
    cor(NumC1, C1),
    corOposta(C1, C2).

% Jog x Bot
decideCorJogadores(Bot, _, C1, C2) :-
    (
        Bot -> decideCorJogador(C2), corOposta(C2, C1); 
        decideCorJogador(C1), corOposta(C1, C2)  
    ).

jogadorVenceu(Jog, Tab) :-
    numPecasBaseFinal(Jog.cor, Tab, Num),
    (
        Num =:= 4 -> !; fail    
    ).

listPecasJogador(Jog, Tab, R) :-
    listPosiMovi(PL),
    listCasas(PL, Tab, PL1),
    append(PL1, PL2),
    include(filterDictKeyValue(cor, Jog.cor), PL2, R).

pecasJogaveis([], _, []).
pecasJogaveis([Peca|T], 6, [Peca|T2]) :-
    posiBaseInicial(Peca.cor, Peca.posi), 
    pecasJogaveis(T, 6, T2).
pecasJogaveis([Peca|T], Dado, [Peca|T2]) :-
    not(posiBaseInicial(Peca.cor, Peca.posi)),
    not(posiBaseFinal(Peca.cor, Peca.posi)),
    pecasJogaveis(T, Dado, T2).
pecasJogaveis([_|T], Dado, T2) :-
    pecasJogaveis(T, Dado, T2).

listaPecasJogaveisJogador(Jog, Dado, Tab, Pecas) :-
    listPecasJogador(Jog, Tab, PecasJog),
    pecasJogaveis(PecasJog, Dado, Pecas).

% quando o jogador é um bot
printJogador(Jog) :-
    Jog.bot,
    printColor(Jog.cor, Jog.cor), 
    printColorCyan(' (BOT)').

% quando o jogador não é um bot
printJogador(Jog) :-
    printColor(Jog.cor, Jog.cor).

printMenuJogador(Ludo, Dado) :-
    cls(),
    nl,nl,nl,
    writeln('\n------------------------------------------------------------------------------------------------------------------------------------------------------------\n'),
    write('Jogador: '), printJogador(Ludo.jogVez), nl,
    write('Dado: '), printColorYellow(Dado), nl,
    writeln('\n------------------------------------------------------------------------- Tabuleiro ------------------------------------------------------------------------\n'),
    printTab(Ludo.tab).

printListaPecas([], _).
printListaPecas([Peca|T], NumPeca) :-
    NewNumPeca is NumPeca + 1,
    printColorCyan('('), printColorCyan(NumPeca), printColorCyan(')'), write(' - '), printPeca(Peca), nl, printListaPecas(T, NewNumPeca).

printListaPecas(Pecas) :- 
    printListaPecas(Pecas, 1).

% tirou 6
mudaJogadorDaVez(Ludo, 6, Ludo).

% Jogador da vez venceu
mudaJogadorDaVez(Ludo, _, Ludo) :-
    jogadorVenceu(Ludo.jogVez, Ludo.tab).

% se jogador da vez é o jog1 muda para o jog2
mudaJogadorDaVez(Ludo, _, NewLudo) :-
    Ludo.jogVez == Ludo.jog1,
    NewLudo = Ludo.put(jogVez, Ludo.jog2).

% se jogador da vez é o jog2 muda para o jog1
mudaJogadorDaVez(Ludo, _, NewLudo) :-
    Ludo.jogVez == Ludo.jog2,
    NewLudo = Ludo.put(jogVez, Ludo.jog1).

movimentaPeca(Ludo, Dado, Peca) :-
    executaMovimentosPeca(Peca, Dado, Ludo.tab, NovoTab),
    NovoLudo = Ludo.put(tab, NovoTab),
    printMenuJogador(NovoLudo, Dado),
    nl,
    write('O jogador '), printJogador(Ludo.jogVez), write(' moveu a peça '), printPeca(Peca),
    nl,
    mudaJogadorDaVez(NovoLudo, Dado, NovoLudo1),
    waitEnterContinue(),
    menuJogo(NovoLudo1).

menuMovimentaPeca(Ludo, Dado) :-
    cls(),
    listaPecasJogaveisJogador(Ludo.jogVez, Dado, Ludo.tab, ListPecas),
    length(ListPecas, LenListPecas),
    LenListPecas \== 0,
    (
        Ludo.jogVez.bot
        ->
            decideJogadaBot(Ludo, Dado, ListPecas, Peca),
            movimentaPeca(Ludo, Dado, Peca)
        ; 
            printMenuJogador(Ludo, Dado),
            printListaPecas(ListPecas),
            printOpcao(),
            readStr(Op),     
            (
                atom_number(Op, OpN), number(OpN), OpN > 0, OpN =< LenListPecas
                -> 
                    nth1(OpN, ListPecas, Peca),
                    movimentaPeca(Ludo, Dado, Peca)
                ;
                    printOpcaoInvalida(),
                    waitEnterBack(),
                    menuMovimentaPeca(Ludo, Dado)
            )
    ).
    
menuMovimentaPeca(Ludo, Dado) :-
    cls(),
    listaPecasJogaveisJogador(Ludo.jogVez, Dado, Ludo.tab, ListPecas),
    length(ListPecas, LenListPecas),
    LenListPecas == 0,
    printMenuJogador(Ludo, Dado),
    nl,nl,
    (
        Ludo.jogVez.bot
        ->
          nl,
          write('O jogador '), printJogador(Ludo.jogVez), write(' não moveu nenhuma peça '),
          nl,
          waitEnterContinue()
        ; 
            nl,
            printColorGreen('Sem opções de peças'),
            nl,
            waitEnterContinue()
      ), 
    mudaJogadorDaVez(Ludo, Dado, NovoLudo),
    menuJogo(NovoLudo).
 
opcaoMenuJogo("1", Ludo) :-
    random_between(1, 6, Dado),
    menuMovimentaPeca(Ludo, Dado).
opcaoMenuJogo("2", Ludo) :-
    menuLudo(Ludo).
opcaoMenuJogo(_, Ludo) :-
    cls(),
    printOpcaoInvalida(),
    menuJogo(Ludo).

menuJogo(Ludo) :-
    cls(),
    (
        jogadorVenceu(Ludo.jogVez, Ludo.tab) 
        -> 
            nl,nl,nl,
            writeln('\n------------------------------------------------------------------------------------------------------------------------------------------------------------\n'),
            write('Jogador: '), printJogador(Ludo.jogVez), nl,
            writeln('\n------------------------------------------------------------------------- Tabuleiro ------------------------------------------------------------------------\n'),
            printTab(Ludo.tab),
            nl,nl,
            write('Vitória do jogador '), printJogador(Ludo.jogVez),
            nl,
            waitEnterBack(),
            menuLudo(Ludo)
        ;
            (
                Ludo.jogVez.bot
                ->
                    Op = "1"
                ;
                    nl,nl,nl,
                    writeln('\n------------------------------------------------------------------------------------------------------------------------------------------------------------\n'),
                    write('Jogador: '), printJogador(Ludo.jogVez), nl,
                    writeln('\n------------------------------------------------------------------------- Tabuleiro ------------------------------------------------------------------------\n'),
                    printTab(Ludo.tab),
                    nl,nl,
                    printColorCyan('(1)'), write(' Jogar Dado'), nl,
                    printColorCyan('(2)'), write(' Voltar'), nl,
                    printOpcao(),
                    readStr(Op)
            ),
            opcaoMenuJogo(Op, Ludo)
    ).
    
iniciaNovoJogo() :-
    Jog1 = jogador{cor: '', bot: false},
    Jog2 = jogador{cor: '', bot: true},

    decideCorJogadores(Jog1.bot, Jog2.bot, CorJog1, CorJog2),
    NewJog1 = Jog1.put(cor, CorJog1),
    NewJog2 = Jog2.put(cor, CorJog2),

    criaTabComPecas(CorJog1, CorJog2, Tab),

    Ludo = ludo{jog1: NewJog1, jog2: NewJog2, tab: Tab, jogVez: NewJog1},
    
    menuJogo(Ludo).

continuarJogo(Ludo) :- 
    (
        is_dict(Ludo)
        ->
            menuJogo(Ludo)
        ;
            cls(),
            nl,
            printColorRed('Não existe nenhum jogo em execução'),
            nl,
            waitEnterBack(),
            menuLudo(Ludo)
    ).

salvarJogo(Ludo) :-
    cls(),
    (
        is_dict(Ludo)
        ->
            dictToJson(Ludo, 'ludo'),
            nl,
            printColorGreen('Jogo salvo com sucesso'),
            nl
        ;
            nl,
            printColorRed('Não existe nenhum jogo em execução'),
            nl
    ),
    waitEnterBack(),
    menuLudo(Ludo).


iniciarJogoSalvo(Ludo) :- 
    cls(),
    (
        existsJson('ludo')
        ->
            jsonToDict(NovoLudo, 'ludo'),
            menuJogo(NovoLudo)
        ;   
            nl,
            printColorRed('Não existe nenhum jogo salvo'),
            nl,
            waitEnterBack(),
            menuLudo(Ludo)
    ).
    
executaOpcaoMenuLudo("1", _) :- iniciaNovoJogo().
executaOpcaoMenuLudo("2", Ludo) :- continuarJogo(Ludo).
executaOpcaoMenuLudo("3", Ludo) :- salvarJogo(Ludo).
executaOpcaoMenuLudo("4", Ludo) :- iniciarJogoSalvo(Ludo).
executaOpcaoMenuLudo("5", _) :- !.
executaOpcaoMenuLudo(_, Ludo) :- printOpcaoInvalida(), menuLudo(Ludo).

menuLudo(Ludo) :-
    cls(),
    printLudoLogo(), nl,
    printColorCyan('(1)'), write(' Novo Jogo'), nl,
    printColorCyan('(2)'), write(' Continuar Jogo'), nl,
    printColorCyan('(3)'), write(' Salvar Jogo'), nl,
    printColorCyan('(4)'), write(' Continuar Jogo Salvo'), nl,
    printColorCyan('(5)'), write(' Voltar'), nl,
    printOpcao(),
    readStr(Op),
    executaOpcaoMenuLudo(Op, Ludo).

dev() :- menuLudo(_).
