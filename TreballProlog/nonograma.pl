% consult(predicatsRecomanats). % inclou el fitxer "predicatsRecomanats.pl"

nonoPrint(NF,NC,IF,IC,G) :- 
    countSet(NF,NF_count), countSet(NC,NC_count), 
    max_list(NF_count,NF_space), max_list(NC_count, NC_space), 
    preprocessIC(NC_space,NC_count,IC,IC_processed), transpose(IC_processed, IC_printable), printHead(NF_space,NC,IC_printable), 
    printMult('-',NF_space), printMult('-',NC), print('\n').

preprocessIC(Space,[This_count|NC_count],[This_IC|IC],[R,Res]) :- processColumn(Space,This_count,This_IC,RAux), reverse(RAux,R), preprocessIC(Space,NC_count,This_IC,Res).

processColumn(0,0,[],[]).
processColumn(Size,0,[],[I|Column]) :- Size > 0, I is ' ', NextSize is Size - 1, processColumn(NextSize,0,[],Column).
processColumn(Size, Length, [H|Hints], [I|Column]) :- Length > 0, I is H, NextSize is Size - 1, NextLength is Length - 1, processColumn(NextSize, NextLength, Hints, Column).

printHead(InitSpace,NC,[R|Rows]) :- printMult(' ',InitSpace), print('|'), printMult(' ',R), print("!\n"), NCnext is NC - 1, printHead(InitSpace,NCnext,Rows).

countSet([],[]).
countSet([I|Input],[R|Result]) :- length(I,R), countSet(Input,Result).

printMult(_,0).
printMult(C,N) :- print(C), Nnext is N - 1, printSpace(Nnext).

printColumn([]).
printColumn([R|G]) :- printRow(R), print('\n'), printColumn(G).

printRow([]).
printRow([X|R]) :- print(X), print(' '), printRow(R).




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