:- module(util, [
    cls/0,
    insertHeadList/3,
    printLudoLogo/0,
    cor/2,
    corOposta/2,
    printColor/2,
    printColorCyan/1,
    printColorRed/1,
    printColorGreen/1,
    printColorYellow/1,
    printColorMagenta/1,
    printOpcaoInvalida/0,
    printOpcao/0,
    readStr/1,
    waitEnterBack/0,
    waitEnterContinue/0,
    filterDictKeyValue/3,
    filterDictNotKeyValue/3,
    getDictKey/3,
    jsonToDict/2,
    dictToJson/2,
    existsJson/1
]).

:- encoding(utf8).
:- use_module(library(http/json)).

cls() :- writeln('\e[H\e[2J').

insertHeadList(H, T, [H|T]).

printLudoLogo() :-
    printColorRed('|----------------------------------------------------------------|'), nl,
    printColorRed('|               ██╗     ██╗   ██╗██████╗  ██████╗                |'), nl,
    printColorRed('|               ██║     ██║   ██║██╔══██╗██╔═══██╗               |'), nl,
    printColorRed('|               ██║     ██║   ██║██║  ██║██║   ██║               |'), nl,
    printColorRed('|               ██║     ██║   ██║██║  ██║██║   ██║               |'), nl,
    printColorRed('|               ███████╗╚██████╔╝██████╔╝╚██████╔╝               |'), nl,
    printColorRed('|               ╚══════╝ ╚═════╝ ╚═════╝  ╚═════╝                |'), nl,
    printColorRed('|----------------------------------------------------------------|'), nl.

cor(1, amarelo).
cor(2, verde).
cor(3, vermelho).
cor(4, azul).

corOposta(amarelo, verde).
corOposta(verde, amarelo).
corOposta(vermelho, azul).
corOposta(azul, vermelho).

getResetColor('\u001b[0m').

printColorRed(Str) :-
    getResetColor(ResetColor),
    write('\u001b[31m'),
    write(Str),
    write(ResetColor).

printColorGreen(Str) :-
    getResetColor(ResetColor),
    write('\u001b[32m'),
    write(Str),
    write(ResetColor).

printColorYellow(Str) :-
    getResetColor(ResetColor),
    write('\u001b[33m'),
    write(Str),
    write(ResetColor).

printColorMagenta(Str) :-
    getResetColor(ResetColor),
    write('\u001b[35m'),
    write(Str),
    write(ResetColor).

printColorCyan(Str) :-
    getResetColor(ResetColor),
    write('\u001b[36m'),
    write(Str),
    write(ResetColor).

printColorWhite(Str) :-
    getResetColor(ResetColor),
    write('\u001b[3m'),
    write(Str),
    write(ResetColor).

printColor(amarelo, Str) :- printColorYellow(Str).
printColor(vermelho, Str) :- printColorRed(Str).
printColor(verde, Str) :- printColorGreen(Str).
printColor(azul, Str) :- printColorCyan(Str).
printColor(branco, Str) :- printColorWhite(Str).

printOpcaoInvalida() :- nl, printColorRed('Opção inválida'), nl.

printOpcao() :- 
    printColorGreen('-----'), nl, 
    write('>> ').

waitEnterBack() :-
    printColorGreen('\nPressione <Enter> para voltar\n'), nl,
    readStr(_).

waitEnterContinue() :-
    printColorGreen('\nPressione <Enter> para continuar\n'), nl,
    readStr(_).

readStr(Str) :- 
    read_line_to_string(user_input, Str).
    
filterDictKeyValue(Key, Value, Dict) :- 
    Dict.Key == Value.

filterDictNotKeyValue(Key, Value, Dict) :- 
    Dict.Key \== Value.

getDictKey(Key, Dict, Dict.Key).

jsonToDict(Dict, JsonName) :-
    string_concat('../data/', JsonName, JsonName1),
    string_concat(JsonName1, '.json', Path),
    open(Path, read, In),
    json_read_dict(In, Dict, [tag(type), value_string_as(atom)]),
    close(In).

dictToJson(Dict, JsonName) :-
    string_concat('../data/', JsonName, JsonName1),
    string_concat(JsonName1, '.json', Path),
    open(Path, write, In),
    json_write_dict(In, Dict, [tag(type)]),
    close(In).

existsJson(JsonName) :- 
    string_concat('../data/', JsonName, JsonName1),
    string_concat(JsonName1, '.json', Path),
    exists_file(Path).
