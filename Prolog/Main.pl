:-[util].

ajuda:-
    writeln('------------------------------------------------------------------------- Como Jogar ------------------------------------------------------------------------'),
    writeln('Objetivo do jogo:'),
    writeln('   * Percorrer todo o trajeto do tabuleiro no sentido horario com todas as peças e ser o primeiro jogador a levar seus quatro peões ao ponto de chegada da sua cor.'),
    writeln(''),
    writeln('Regras:'),
    writeln('   * Pode ser jogado por um player e um bot baseado em heurística e aleatoriedade.'),
    writeln('   * O tabuleiro quadrado tem um percurso em forma de cruz e cada jogador tem quatro peões.'),
    writeln('   * Os peões de cada jogador começam na base de mesma cor.'),
    writeln('   * Para se iniciar a partida, joga-se o dado e o participante que fizer o maior número de pontos (6) inicia o jogo retirando uma peça da base'),
    writeln('   * Quando o jogador já tem pelo menos um peão no percurso, joga-se um dado e os avanços são feitos de acordo com os pontos obtidos com o lançamento do dado. Se tirar 6, além de usar esse resultado ele pode jogar novamente o dado.'),
    writeln('   * Para transportar um peão de sua base para seu ponto de partida é necessário tirar (6).'),
    writeln('   * Se um jogador chegar a uma casa já ocupada por um peão adversário, o peão adversário deve voltar para sua base.'),
    writeln('   * Mas se 2 peões da mesma cor ocuparem uma mesma casa, eles não podem ser capturados e nenhum adversário pode passar por essa casa, tendo seus peões bloqueados.'),
    writeln('   * Após dar a volta no tabuleiro o peão avança pela reta final, de sua própria cor. A chegada ao ponto final só pode ser obtida por um número exato nos dados. Se o jogador tirar mais do que o necessário, ele vai até o fim e volta, tendo que aguardar sua próxima jogada.'),
    writeln(''),
    writeln('Outras Funcionalidades:'),
    writeln('   * O jogo terá um sistema de salvamento;'),
    writeln('   * Terá obstáculos pelo caminho como uma casa que retrocede duas casas;'),
    writeln(''),
    writeln(''),
    writeln(''),
    writeln('Pressione <Enter> para voltar').

creditos:-
    writeln('------------------------------------------------------------------------- Desenvolvedores ------------------------------------------------------------------------'),
    writeln('Felipe Oliveira'),
    writeln('Lucas Lima')
    writeln('Vinícius Lins')
    writeln('Pedro Manoel'),
    writeln('Pressione <Enter> para voltar').

sair:-
    writeln('Obrigado por jogar').

jogar.

executaOpcaoMain(1) :- jogar().
executaOpcaoMain(2) :- ajuda().
executaOpcaoMain(3) :- creditos().
executaOpcaoMain(4) :- sair().
executaOpcaoMain(_) :- writeln('opção inválida'), main.

cls :- write('\e[H\e[2J').

main :-
    ludoLogo,

    writeln('(1) Jogar'),
    writeln('(2) Ajuda'),
    writeln('(3) Credítos'),
    writeln('(4) Sair'),
    read(Op),
    executaOpcaoMain(Op),
    halt.

