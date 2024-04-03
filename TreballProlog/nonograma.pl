% consult(predicatsRecomanats). % inclou el fitxer "predicatsRecomanats.pl"

nonoPrint(NF,NC,IF,IC,G) :- 
    countSet(IF,IF_count), countSet(IC,IC_count), 
    max_list(IF_count,IF_space), max_list(IC_count, IC_space), 
    preprocessIC(IC_space,IC_count,IC,IC_processed), transpose(IC_processed, IC_printable),
    printHead(2*IF_space-1,IC_space,IC_printable), 
    printMult('-',2*IF_space), printMult('-',2*NC-1), print('\n'),
    preprocessIC(IF_space,IF_count,IF,IF_printable),
    printBody(IF_printable,G), !.

preprocessIC(_,[],[],[]).
preprocessIC(Space,[This_count|NC_count],[This_IC|IC],[R|Res]) :- This_IC \= [], processColumn(Space,This_count,This_IC,RAux), reverse(RAux,R), preprocessIC(Space,NC_count,IC,Res).

processColumn(0,0,[],[]).
processColumn(Size,0,[],[I|Column]) :- Size > 0, I = ' ', NextSize is Size - 1, processColumn(NextSize,0,[],Column).
processColumn(Size, Length, [H|Hints], [I|Column]) :- Length > 0, I is H, NextSize is Size - 1, NextLength is Length - 1, processColumn(NextSize, NextLength, Hints, Column).

printHead(_,0,[]).
printHead(InitSpace,NC,[R|Rows]) :- NC > 0, printMult(' ',InitSpace), print('|'), printRow(R), print('\n'), NCnext is NC - 1, printHead(InitSpace,NCnext,Rows).

countSet([],[]).
countSet([I|Input],[R|Result]) :- length(I,R), countSet(Input,Result).

printMult(_,0).
printMult(C,N) :- N > 0, print(C), Nnext is N - 1, printMult(C,Nnext).

printBody([],[]).
printBody([H|Hints],[R|G]) :- printRow(H), print('|'), printRow(R), print('\n'), printBody(Hints,G).

printRow([]).
printRow([X|R]) :- print(X), printEnd(R), printRow(R).
printEnd([]).
printEnd(_) :- print(' ').




%transpose(+Xs,Ys): Ys es la matriu (llista de llistes) Xs transposada. 
% Assumim que es una matriu ben construida, es a dir, totes les files (subllistes) tenen la mateixa llargada.
transpose([[]|_], []). %cal posar el [ []|_ ] perquè és el cas base (una de les files de la matriu és buida).
transpose(Matrix, [Row|Rows]) :- transPrimeraColumna(Matrix, Row, RestMatrix), 
                                 transpose(RestMatrix, Rows).
                                % transposem la primera columna de la matriu i continuem amb la resta de la matriu i files.

transPrimeraColumna([], [], []). %cas base, la transposada d'una llista buida és una llista buida.
transPrimeraColumna([[H|T]|Rows], [H|Hs], [T|Ts]) :- transPrimeraColumna(Rows, Hs, Ts). % cas recursiu, agafem el primer element de la primera llista (H), l'afegim al resultat (H|Hs)
                                                                                    % i la resta de la primera llista ho col·loquem a la continuació de la matriu (T|Ts).
                                                                                    % llavors només queda cridar el mateix per a transposar la següent fila de la matriu.
