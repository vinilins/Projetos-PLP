:- module(main, []).

%:- initialization(mainLudo).
:- encoding(utf8).

:- use_module('src/util.pl').
:- use_module('src/ludo.pl').

ajuda() :-
    cls(),
    writeln('\n------------------------------------------------------------------------- Como Jogar ------------------------------------------------------------------------\n'),
    writeln('Objetivo do jogo:'),
    writeln('   * Percorrer todo o trajeto do tabuleiro no sentido horario com todas as peças e ser o primeiro jogador'), 
    writeln('     a levar seus quatro peões ao ponto de chegada da sua cor.'), nl,
    writeln('Regras:'),
    writeln('   * Pode ser jogado por um player e um bot baseado em heurística e aleatoriedade.'), nl,
    writeln('   * O tabuleiro quadrado tem um percurso em forma de cruz e cada jogador tem quatro peões.'), nl,
    writeln('   * Os peões de cada jogador começam na base de mesma cor.'), nl,
    writeln('   * Para se iniciar a partida, joga-se o dado e o participante que fizer o maior número de pontos (6)'), 
    writeln('     inicia o jogo retirando uma peça da base'), nl,
    writeln('   * Quando o jogador já tem pelo menos um peão no percurso, joga-se um dado e os avanços são feitos'), 
    writeln('     de acordo com os pontos obtidos com o lançamento do dado. Se tirar 6, além de usar esse resultado'),
    writeln('     ele pode jogar novamente o dado.'), nl,
    writeln('   * Para transportar um peão de sua base para seu ponto de partida é necessário tirar (6).'), nl,
    writeln('   * Se um jogador chegar a uma casa já ocupada por um peão adversário, o peão adversário deve voltar'), 
    writeln('     para sua base.'), nl,
    writeln('   * Mas se 2 peões da mesma cor ocuparem uma mesma casa, eles não podem ser capturados e nenhum adversário'), 
    writeln('     pode passar por essa casa, tendo seus peões bloqueados.'), nl,
    writeln('   * Após dar a volta no tabuleiro o peão avança pela reta final, de sua própria cor. A chegada ao ponto'), 
    writeln('     final só pode ser obtida por um número exato nos dados. Se o jogador tirar mais do que o necessário,'), 
    writeln('     ele vai até o fim e volta, tendo que aguardar sua próxima jogada.'), nl,
    writeln('Outras Funcionalidades:'),
    writeln('   * O jogo terá um sistema de salvamento;'), nl,
    writeln('   * Terá obstáculos pelo caminho como uma casa que retrocede duas casas;'),
    waitEnterBack(),
    mainLudo().

creditos() :-
    cls(),
    writeln('\n------------------------------------------------------------------------- Desenvolvedores ------------------------------------------------------------------------\n'),
    printColorCyan('Felipe Oliveira'), nl,
    printColorCyan('Lucas Lima'), nl,
    printColorCyan('Vinícius Lins'), nl,
    printColorCyan('Pedro Manoel'), nl,
    waitEnterBack(),
    mainLudo().

sair() :- nl, printColorCyan('Obrigado por jogar'), !.

jogar() :- 
    menuLudo(_),
    mainLudo().

executaOpcaoMain("1") :- jogar().
executaOpcaoMain("2") :- ajuda().
executaOpcaoMain("3") :- creditos().
executaOpcaoMain("4") :- sair().
executaOpcaoMain(_) :- printOpcaoInvalida(), mainLudo().

mainLudo() :-
    cls(),
    printLudoLogo(), nl,
    printColorCyan('(1)'), write(' Jogar'), nl,
    printColorCyan('(2)'), write(' Ajuda'), nl,
    printColorCyan('(3)'), write(' Credítos'), nl,
    printColorCyan('(4)'), write(' Sair'), nl,
    printOpcao(),
    readStr(Op),
    executaOpcaoMain(Op),
    halt.

