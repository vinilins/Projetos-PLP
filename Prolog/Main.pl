ajuda.
creditos.
sair.
jogar.

executaOpcaoMain(1) :- jogar().
executaOpcaoMain(2) :- ajuda().
executaOpcaoMain(3) :- creditos().
executaOpcaoMain(4) :- sair().
executaOpcaoMain(_) :- writeln('opção inválida'), main.

cls :- write('\e[H\e[2J').

main :-
    writeln('(1) Jogar'),
    writeln('(2) Ajuda'),
    writeln('(3) Credítos'),
    writeln('(4) Sair'),
    read(Op),
    executaOpcaoMain(Op),
    halt.

