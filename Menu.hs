import System.Process

ajuda :: IO()
ajuda  = do
    putStrLn "Para se iniciar a partida, joga-se o dado e o participante que fizer o maior numero de pontos (6) inicia o jogo, continuando as jogadas em sentido horario. Joga-se com um dado e os avancos sao feitos de acordo com os pontos obtidos com o lancamento dos dados. Cada jogador lanca o dado e se tirar (6) podera sair da casa de partida, sendo que, ao tirar o mesmo, voce tem o direito de jogar o dado novamente. Pode ser jogado por um player e um bot baseado em heuristica e aleatoriedade. O tabuleiro quadrado tem um percurso em forma de cruz e cada jogador tem quatro peoes. Um dado define os movimentos. Os peoes de cada jogador comecam na base de mesma cor. O objetivo do jogo eh ser o primeiro a levar seus 4 peoes a dar uma volta no tabuleiro e a chegar no ponto final marcado com sua cor. Os peoes movem-se pelo percurso no sentido horario. Para transportar um peao de sua base para seu ponto de partida eh necessario tirar 6. Quando o jogador ja tem pelo menos um peao no percurso, ele pode mover o peao do numero de casas tirado no dado. Se tirar 6, alem de usar esse resultado ele pode jogar novamente o dado. Se um jogador chegar a uma casa ja ocupada por um peao adversario, o peao adversario deve voltar para sua base. Mas se 2 peoes da mesma cor ocuparem uma mesma casa, eles nao podem ser capturados e nenhum adversario pode passar por essa casa, tendo seus peoes bloqueados. Apos dar a volta no tabuleiro o peao avanca pela reta final, de sua propria cor. A chegada ao ponto final so pode ser obtida por um numero exato nos dados. Se o jogador tirar mais do que o necessario, ele vai ate o fim e volta, tendo que aguardar sua proxima jogada. O vencedor eh o primeiro a levar seus quatro peoes ao ponto de chegada da sua cor.\n"
    main

creditos :: IO()
creditos  = do
    putStrLn "Feito por: Emanuel Moura, Felipe Oliveira, Lucas Lima, Pedro Herminio, Vinicius Lins\n"
    main


sairBunitinho :: IO ()
sairBunitinho = do
    putStrLn "Encerrando o programa..."
    system "cls"
    putStrLn "Programa encerrado"

executaOpcao :: Int -> IO()
executaOpcao opcao
    | (opcao == 1) = putStrLn "1"
    | (opcao == 2) = putStrLn "2"
    | (opcao == 3) = ajuda
    | (opcao == 4) = creditos
    | (opcao == 5) = sairBunitinho
    | otherwise = putStrLn "Opcao invalida, digite novamente"



main :: IO ()
main = do
    --system "cls" -- limpa a tela somente no windows
    putStrLn "-------------------------------- Jogo do LUDO --------------------------------"
    putStrLn "(1) Novo Jogo"
    putStrLn "(2) Continuar"
    putStrLn "(3) Ajuda"
    putStrLn "(4) Creditos"
    putStrLn "(5) Sair"
    op <- getLine
    executaOpcao (read op)

