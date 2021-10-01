:- module(posicao, [
    posiBaseInicial/2,
    posiBaseFinal/2,
    posiVoltaDuasCasas/1,
    posiTerritorioCor/2,
    isPosiMovi/1,
    listPosiMovi/1,
    criaPosi/3,
    matrizPosiTab/1
]).

listPosiTab(R) :-
    R = [
            posi{lin: 1, col: 1}, posi{lin: 1, col: 2}, posi{lin: 1, col: 3}, posi{lin: 1, col: 4}, posi{lin: 1, col: 5}, posi{lin: 1, col: 6}, posi{lin: 1, col: 7}, posi{lin: 1, col: 8}, posi{lin: 1, col: 9}, posi{lin: 1, col: 10}, posi{lin: 1, col: 11}, posi{lin: 1, col: 12}, posi{lin: 1, col: 13}, posi{lin: 1, col: 14}, posi{lin: 1, col: 15},
            posi{lin: 2, col: 1}, posi{lin: 2, col: 2}, posi{lin: 2, col: 3}, posi{lin: 2, col: 4}, posi{lin: 2, col: 5}, posi{lin: 2, col: 6}, posi{lin: 2, col: 7}, posi{lin: 2, col: 8}, posi{lin: 2, col: 9}, posi{lin: 2, col: 10}, posi{lin: 2, col: 11}, posi{lin: 2, col: 12}, posi{lin: 2, col: 13}, posi{lin: 2, col: 14}, posi{lin: 2, col: 15},
            posi{lin: 3, col: 1}, posi{lin: 3, col: 2}, posi{lin: 3, col: 3}, posi{lin: 3, col: 4}, posi{lin: 3, col: 5}, posi{lin: 3, col: 6}, posi{lin: 3, col: 7}, posi{lin: 3, col: 8}, posi{lin: 3, col: 9}, posi{lin: 3, col: 10}, posi{lin: 3, col: 11}, posi{lin: 3, col: 12}, posi{lin: 3, col: 13}, posi{lin: 3, col: 14}, posi{lin: 3, col: 15},
            posi{lin: 4, col: 1}, posi{lin: 4, col: 2}, posi{lin: 4, col: 3}, posi{lin: 4, col: 4}, posi{lin: 4, col: 5}, posi{lin: 4, col: 6}, posi{lin: 4, col: 7}, posi{lin: 4, col: 8}, posi{lin: 4, col: 9}, posi{lin: 4, col: 10}, posi{lin: 4, col: 11}, posi{lin: 4, col: 12}, posi{lin: 4, col: 13}, posi{lin: 4, col: 14}, posi{lin: 4, col: 15},
            posi{lin: 5, col: 1}, posi{lin: 5, col: 2}, posi{lin: 5, col: 3}, posi{lin: 5, col: 4}, posi{lin: 5, col: 5}, posi{lin: 5, col: 6}, posi{lin: 5, col: 7}, posi{lin: 5, col: 8}, posi{lin: 5, col: 9}, posi{lin: 5, col: 10}, posi{lin: 5, col: 11}, posi{lin: 5, col: 12}, posi{lin: 5, col: 13}, posi{lin: 5, col: 14}, posi{lin: 5, col: 15},
            posi{lin: 6, col: 1}, posi{lin: 6, col: 2}, posi{lin: 6, col: 3}, posi{lin: 6, col: 4}, posi{lin: 6, col: 5}, posi{lin: 6, col: 6}, posi{lin: 6, col: 7}, posi{lin: 6, col: 8}, posi{lin: 6, col: 9}, posi{lin: 6, col: 10}, posi{lin: 6, col: 11}, posi{lin: 6, col: 12}, posi{lin: 6, col: 13}, posi{lin: 6, col: 14}, posi{lin: 6, col: 15},
            posi{lin: 7, col: 1}, posi{lin: 7, col: 2}, posi{lin: 7, col: 3}, posi{lin: 7, col: 4}, posi{lin: 7, col: 5}, posi{lin: 7, col: 6}, posi{lin: 7, col: 7}, posi{lin: 7, col: 8}, posi{lin: 7, col: 9}, posi{lin: 7, col: 10}, posi{lin: 7, col: 11}, posi{lin: 7, col: 12}, posi{lin: 7, col: 13}, posi{lin: 7, col: 14}, posi{lin: 7, col: 15},
            posi{lin: 8, col: 1}, posi{lin: 8, col: 2}, posi{lin: 8, col: 3}, posi{lin: 8, col: 4}, posi{lin: 8, col: 5}, posi{lin: 8, col: 6}, posi{lin: 8, col: 7}, posi{lin: 8, col: 8}, posi{lin: 8, col: 9}, posi{lin: 8, col: 10}, posi{lin: 8, col: 11}, posi{lin: 8, col: 12}, posi{lin: 8, col: 13}, posi{lin: 8, col: 14}, posi{lin: 8, col: 15},
            posi{lin: 9, col: 1}, posi{lin: 9, col: 2}, posi{lin: 9, col: 3}, posi{lin: 9, col: 4}, posi{lin: 9, col: 5}, posi{lin: 9, col: 6}, posi{lin: 9, col: 7}, posi{lin: 9, col: 8}, posi{lin: 9, col: 9}, posi{lin: 9, col: 10}, posi{lin: 9, col: 11}, posi{lin: 9, col: 12}, posi{lin: 9, col: 13}, posi{lin: 9, col: 14}, posi{lin: 9, col: 15},
            posi{lin: 10, col: 1}, posi{lin: 10, col: 2}, posi{lin: 10, col: 3}, posi{lin: 10, col: 4}, posi{lin: 10, col: 5}, posi{lin: 10, col: 6}, posi{lin: 10, col: 7}, posi{lin: 10, col: 8}, posi{lin: 10, col: 9}, posi{lin: 10, col: 10}, posi{lin: 10, col: 11}, posi{lin: 10, col: 12}, posi{lin: 10, col: 13}, posi{lin: 10, col: 14}, posi{lin: 10, col: 15},
            posi{lin: 11, col: 1}, posi{lin: 11, col: 2}, posi{lin: 11, col: 3}, posi{lin: 11, col: 4}, posi{lin: 11, col: 5}, posi{lin: 11, col: 6}, posi{lin: 11, col: 7}, posi{lin: 11, col: 8}, posi{lin: 11, col: 9}, posi{lin: 11, col: 10}, posi{lin: 11, col: 11}, posi{lin: 11, col: 12}, posi{lin: 11, col: 13}, posi{lin: 11, col: 14}, posi{lin: 11, col: 15},
            posi{lin: 12, col: 1}, posi{lin: 12, col: 2}, posi{lin: 12, col: 3}, posi{lin: 12, col: 4}, posi{lin: 12, col: 5}, posi{lin: 12, col: 6}, posi{lin: 12, col: 7}, posi{lin: 12, col: 8}, posi{lin: 12, col: 9}, posi{lin: 12, col: 10}, posi{lin: 12, col: 11}, posi{lin: 12, col: 12}, posi{lin: 12, col: 13}, posi{lin: 12, col: 14}, posi{lin: 12, col: 15},
            posi{lin: 13, col: 1}, posi{lin: 13, col: 2}, posi{lin: 13, col: 3}, posi{lin: 13, col: 4}, posi{lin: 13, col: 5}, posi{lin: 13, col: 6}, posi{lin: 13, col: 7}, posi{lin: 13, col: 8}, posi{lin: 13, col: 9}, posi{lin: 13, col: 10}, posi{lin: 13, col: 11}, posi{lin: 13, col: 12}, posi{lin: 13, col: 13}, posi{lin: 13, col: 14}, posi{lin: 13, col: 15},
            posi{lin: 14, col: 1}, posi{lin: 14, col: 2}, posi{lin: 14, col: 3}, posi{lin: 14, col: 4}, posi{lin: 14, col: 5}, posi{lin: 14, col: 6}, posi{lin: 14, col: 7}, posi{lin: 14, col: 8}, posi{lin: 14, col: 9}, posi{lin: 14, col: 10}, posi{lin: 14, col: 11}, posi{lin: 14, col: 12}, posi{lin: 14, col: 13}, posi{lin: 14, col: 14}, posi{lin: 14, col: 15},
            posi{lin: 15, col: 1}, posi{lin: 15, col: 2}, posi{lin: 15, col: 3}, posi{lin: 15, col: 4}, posi{lin: 15, col: 5}, posi{lin: 15, col: 6}, posi{lin: 15, col: 7}, posi{lin: 15, col: 8}, posi{lin: 15, col: 9}, posi{lin: 15, col: 10}, posi{lin: 15, col: 11}, posi{lin: 15, col: 12}, posi{lin: 15, col: 13}, posi{lin: 15, col: 14}, posi{lin: 15, col: 15} 
        ].
    
matrizPosiTab(R) :-
    R = [
            [posi{lin: 1, col: 1}, posi{lin: 1, col: 2}, posi{lin: 1, col: 3}, posi{lin: 1, col: 4}, posi{lin: 1, col: 5}, posi{lin: 1, col: 6}, posi{lin: 1, col: 7}, posi{lin: 1, col: 8}, posi{lin: 1, col: 9}, posi{lin: 1, col: 10}, posi{lin: 1, col: 11}, posi{lin: 1, col: 12}, posi{lin: 1, col: 13}, posi{lin: 1, col: 14}, posi{lin: 1, col: 15}],
            [posi{lin: 2, col: 1}, posi{lin: 2, col: 2}, posi{lin: 2, col: 3}, posi{lin: 2, col: 4}, posi{lin: 2, col: 5}, posi{lin: 2, col: 6}, posi{lin: 2, col: 7}, posi{lin: 2, col: 8}, posi{lin: 2, col: 9}, posi{lin: 2, col: 10}, posi{lin: 2, col: 11}, posi{lin: 2, col: 12}, posi{lin: 2, col: 13}, posi{lin: 2, col: 14}, posi{lin: 2, col: 15}],
            [posi{lin: 3, col: 1}, posi{lin: 3, col: 2}, posi{lin: 3, col: 3}, posi{lin: 3, col: 4}, posi{lin: 3, col: 5}, posi{lin: 3, col: 6}, posi{lin: 3, col: 7}, posi{lin: 3, col: 8}, posi{lin: 3, col: 9}, posi{lin: 3, col: 10}, posi{lin: 3, col: 11}, posi{lin: 3, col: 12}, posi{lin: 3, col: 13}, posi{lin: 3, col: 14}, posi{lin: 3, col: 15}],
            [posi{lin: 4, col: 1}, posi{lin: 4, col: 2}, posi{lin: 4, col: 3}, posi{lin: 4, col: 4}, posi{lin: 4, col: 5}, posi{lin: 4, col: 6}, posi{lin: 4, col: 7}, posi{lin: 4, col: 8}, posi{lin: 4, col: 9}, posi{lin: 4, col: 10}, posi{lin: 4, col: 11}, posi{lin: 4, col: 12}, posi{lin: 4, col: 13}, posi{lin: 4, col: 14}, posi{lin: 4, col: 15}],
            [posi{lin: 5, col: 1}, posi{lin: 5, col: 2}, posi{lin: 5, col: 3}, posi{lin: 5, col: 4}, posi{lin: 5, col: 5}, posi{lin: 5, col: 6}, posi{lin: 5, col: 7}, posi{lin: 5, col: 8}, posi{lin: 5, col: 9}, posi{lin: 5, col: 10}, posi{lin: 5, col: 11}, posi{lin: 5, col: 12}, posi{lin: 5, col: 13}, posi{lin: 5, col: 14}, posi{lin: 5, col: 15}],
            [posi{lin: 6, col: 1}, posi{lin: 6, col: 2}, posi{lin: 6, col: 3}, posi{lin: 6, col: 4}, posi{lin: 6, col: 5}, posi{lin: 6, col: 6}, posi{lin: 6, col: 7}, posi{lin: 6, col: 8}, posi{lin: 6, col: 9}, posi{lin: 6, col: 10}, posi{lin: 6, col: 11}, posi{lin: 6, col: 12}, posi{lin: 6, col: 13}, posi{lin: 6, col: 14}, posi{lin: 6, col: 15}],
            [posi{lin: 7, col: 1}, posi{lin: 7, col: 2}, posi{lin: 7, col: 3}, posi{lin: 7, col: 4}, posi{lin: 7, col: 5}, posi{lin: 7, col: 6}, posi{lin: 7, col: 7}, posi{lin: 7, col: 8}, posi{lin: 7, col: 9}, posi{lin: 7, col: 10}, posi{lin: 7, col: 11}, posi{lin: 7, col: 12}, posi{lin: 7, col: 13}, posi{lin: 7, col: 14}, posi{lin: 7, col: 15}],
            [posi{lin: 8, col: 1}, posi{lin: 8, col: 2}, posi{lin: 8, col: 3}, posi{lin: 8, col: 4}, posi{lin: 8, col: 5}, posi{lin: 8, col: 6}, posi{lin: 8, col: 7}, posi{lin: 8, col: 8}, posi{lin: 8, col: 9}, posi{lin: 8, col: 10}, posi{lin: 8, col: 11}, posi{lin: 8, col: 12}, posi{lin: 8, col: 13}, posi{lin: 8, col: 14}, posi{lin: 8, col: 15}],
            [posi{lin: 9, col: 1}, posi{lin: 9, col: 2}, posi{lin: 9, col: 3}, posi{lin: 9, col: 4}, posi{lin: 9, col: 5}, posi{lin: 9, col: 6}, posi{lin: 9, col: 7}, posi{lin: 9, col: 8}, posi{lin: 9, col: 9}, posi{lin: 9, col: 10}, posi{lin: 9, col: 11}, posi{lin: 9, col: 12}, posi{lin: 9, col: 13}, posi{lin: 9, col: 14}, posi{lin: 9, col: 15}],
            [posi{lin: 10, col: 1}, posi{lin: 10, col: 2}, posi{lin: 10, col: 3}, posi{lin: 10, col: 4}, posi{lin: 10, col: 5}, posi{lin: 10, col: 6}, posi{lin: 10, col: 7}, posi{lin: 10, col: 8}, posi{lin: 10, col: 9}, posi{lin: 10, col: 10}, posi{lin: 10, col: 11}, posi{lin: 10, col: 12}, posi{lin: 10, col: 13}, posi{lin: 10, col: 14}, posi{lin: 10, col: 15}],
            [posi{lin: 11, col: 1}, posi{lin: 11, col: 2}, posi{lin: 11, col: 3}, posi{lin: 11, col: 4}, posi{lin: 11, col: 5}, posi{lin: 11, col: 6}, posi{lin: 11, col: 7}, posi{lin: 11, col: 8}, posi{lin: 11, col: 9}, posi{lin: 11, col: 10}, posi{lin: 11, col: 11}, posi{lin: 11, col: 12}, posi{lin: 11, col: 13}, posi{lin: 11, col: 14}, posi{lin: 11, col: 15}],
            [posi{lin: 12, col: 1}, posi{lin: 12, col: 2}, posi{lin: 12, col: 3}, posi{lin: 12, col: 4}, posi{lin: 12, col: 5}, posi{lin: 12, col: 6}, posi{lin: 12, col: 7}, posi{lin: 12, col: 8}, posi{lin: 12, col: 9}, posi{lin: 12, col: 10}, posi{lin: 12, col: 11}, posi{lin: 12, col: 12}, posi{lin: 12, col: 13}, posi{lin: 12, col: 14}, posi{lin: 12, col: 15}],
            [posi{lin: 13, col: 1}, posi{lin: 13, col: 2}, posi{lin: 13, col: 3}, posi{lin: 13, col: 4}, posi{lin: 13, col: 5}, posi{lin: 13, col: 6}, posi{lin: 13, col: 7}, posi{lin: 13, col: 8}, posi{lin: 13, col: 9}, posi{lin: 13, col: 10}, posi{lin: 13, col: 11}, posi{lin: 13, col: 12}, posi{lin: 13, col: 13}, posi{lin: 13, col: 14}, posi{lin: 13, col: 15}],
            [posi{lin: 14, col: 1}, posi{lin: 14, col: 2}, posi{lin: 14, col: 3}, posi{lin: 14, col: 4}, posi{lin: 14, col: 5}, posi{lin: 14, col: 6}, posi{lin: 14, col: 7}, posi{lin: 14, col: 8}, posi{lin: 14, col: 9}, posi{lin: 14, col: 10}, posi{lin: 14, col: 11}, posi{lin: 14, col: 12}, posi{lin: 14, col: 13}, posi{lin: 14, col: 14}, posi{lin: 14, col: 15}],
            [posi{lin: 15, col: 1}, posi{lin: 15, col: 2}, posi{lin: 15, col: 3}, posi{lin: 15, col: 4}, posi{lin: 15, col: 5}, posi{lin: 15, col: 6}, posi{lin: 15, col: 7}, posi{lin: 15, col: 8}, posi{lin: 15, col: 9}, posi{lin: 15, col: 10}, posi{lin: 15, col: 11}, posi{lin: 15, col: 12}, posi{lin: 15, col: 13}, posi{lin: 15, col: 14}, posi{lin: 15, col: 15}] 
        ].

posiBaseInicial(amarelo, posi{lin: 14, col: 6}).
posiBaseInicial(vermelho, posi{lin: 6, col: 2}).
posiBaseInicial(verde, posi{lin: 2, col: 10}).
posiBaseInicial(azul, posi{lin: 10, col: 14}).

posiBaseFinal(amarelo, posi{lin: 9, col: 8}).
posiBaseFinal(vermelho, posi{lin: 8, col: 7}).
posiBaseFinal(verde, posi{lin: 7, col: 8}).
posiBaseFinal(azul, posi{lin: 8, col: 9}).

posiVoltaDuasCasas(posi{lin: 13, col: 9}).
posiVoltaDuasCasas(posi{lin: 9, col: 3}).
posiVoltaDuasCasas(posi{lin: 7, col: 13}).
posiVoltaDuasCasas(posi{lin: 3, col: 7}).

posiTerritorioAmarelo(Posi) :-
    posiBaseInicial(amarelo, Posi);
    posiBaseFinal(amarelo, Posi);
    Posi == posi{lin: 14, col:7};
    (Posi.col == 8, member(Posi.lin, [10, 11, 12, 13, 14])).

posiTerritorioVermelho(Posi) :- 
    posiBaseInicial(vermelho, Posi);
    posiBaseFinal(vermelho, Posi);
    Posi == posi{lin: 7, col:2};
    (Posi.lin == 8, member(Posi.col, [2, 3, 4, 5, 6, 7])).

posiTerritorioVerde(Posi) :-
    posiBaseInicial(verde, Posi);
    posiBaseFinal(verde, Posi);
    Posi == posi{lin: 2, col:9};
    (Posi.col == 8, member(Posi.lin, [2, 3, 4, 5, 6, 7])). 

posiTerritorioAzul(Posi) :- 
    posiBaseInicial(azul, Posi);
    posiBaseFinal(azul, Posi);
    Posi == posi{lin: 9, col:14};
    (Posi.lin == 8, member(Posi.col, [10, 11, 12, 13, 14])).

posiTerritorioCor(Posi, amarelo) :- posiTerritorioAmarelo(Posi).
posiTerritorioCor(Posi, vermelho) :- posiTerritorioVermelho(Posi).
posiTerritorioCor(Posi, verde) :- posiTerritorioVerde(Posi).
posiTerritorioCor(Posi, azul) :- posiTerritorioAzul(Posi).
posiTerritorioCor(_, branco).

isPosiMovi(Posi) :-
    not(member(Posi, [posi{lin: 8, col: 8}, posi{lin: 7, col: 7}, posi{lin: 9, col: 9}, posi{lin: 9, col: 7}, posi{lin: 7, col: 9}])),
    (
        member(Posi.lin, [7,8,9]); 
        member(Posi.col, [7,8,9]);
        posiBaseInicial(_, Posi)
    ).

listPosiMovi(R) :-
    listPosiTab(LP),
    include(isPosiMovi, LP, R).

criaPosi(Lin, Col, Posi) :-
    Posi = posi{lin: Lin, col: Col}.
