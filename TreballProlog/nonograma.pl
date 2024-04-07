%rowDef(Hints,Ts,+Len) : Hints es la llista d'indicacions per la fila Ts, que te llargada Len. ¡¡ Ni NHints ni Ts cal que estiguin instanciats !!
rowDef([],[],0).

rowDef(Hints, Ts, Len):-
    length(Ts,Len),
    countSublists(Ts,[],Hints).

countSublists([],[],[]).

countSublists([], Sublist, [Count]) :- %CAS ESPECIAL QUAN LA LLISTA ACABA EN 'x'
    Sublist \= [],  %ens assegurem que almenys hi ha alguna 'x'
    length(Sublist, Count).

countSublists([' '|Ts], [], CountVect) :- %QUAN TROBEM UN ' ' I VENIM D'UN ' '
    countSublists(Ts, [], CountVect).

countSublists(['x'|Ts], Sublist, CountVect) :- %TROBEM UNA 'x', ANEM FENT APPEND FINS QUE ARRIBI UN ' ' (cas de sota)
    append(Sublist, ['x'], NewSublist),
    countSublists(Ts, NewSublist, CountVect).

countSublists([' '|Ts], Sublist, [Count|CountVect]) :- %QUAN TROBEM UN ' ' I VENIM DE UNA 'x'
    Sublist \= [],
    length(Sublist, Count),
    countSublists(Ts, [], CountVect).

%transpose(+Xs,Ys): Ys es la matriu (llista de llistes) Xs transposada. 
% Assumim que es una matriu ben construida, es a dir, totes les files (subllistes) tenen la mateixa llargada.
transpose([],[]):- !.
transpose([[]|_], []). %cal posar el [ []|_ ] perquè és el cas base (una de les files de la matriu és buida).
transpose(Matrix, [Row|Rows]) :- transPrimeraColumna(Matrix, Row, RestMatrix), 
                                 transpose(RestMatrix, Rows).
                                % transposem la primera columna de la matriu i continuem amb la resta de la matriu i files.

transPrimeraColumna([], [], []). %cas base, la transposada d'una llista buida és una llista buida.
transPrimeraColumna([[H|T]|Rows], [H|Hs], [T|Ts]) :- transPrimeraColumna(Rows, Hs, Ts). % cas recursiu, agafem el primer element de la primera llista (H), l'afegim al resultat (H|Hs)
                                                                                    % i la resta de la primera llista ho col·loquem a la continuació de la matriu (T|Ts).
                                                                                    % llavors només queda cridar el mateix per a transposar la següent fila de la matriu.

% ============================================ NONOBIDIREC =============================================
generarFila(Len,[],X).
generarFila(Len, [Pista|Pistes],[Fila|Files]):-
    rowDef(Pista, Fila, Len),
    generarFila(Len, Pistes, Files).

nonoBidirec(NF,NC,IF,IC,G):-
    length(IF,NC),
    length(IC,NF),
    length(G,NF),
    generarFila(NC,IF,G),
    length(Columnes, NC),
    transpose(G,Columnes),
    generarFila(NF,IC,Columnes).

nonoBidirecPrint(NF,NC,IF,IC,G):-
    nonoBidirec(NF,NC,IF,IC,G),
    nonoPrint(NF,NC,IF,IC,G).
% ======================================================================================================

% ============================================= WRAPPERS ===============================================

solucionarNono(NF,NC,IF,IC,G) :- nonoBidirec(NF,NC,IF,IC,G).
solucionarNonoPrint(NF,NC,IF,IC,G) :- nonoBidirecPrint(NF,NC,IF,IC,G).
generarNono(NF,NC,IF,IC,G) :- nonoBidirec(NF,NC,IF,IC,G).
generarNonoPrint(NF,NC,IF,IC,G) :- nonoBidirecPrint(NF,NC,IF,IC,G).

% ======================================================================================================

% =============================================== PRINT ================================================
nonoPrint(NF,NC,IF,IC,G) :- 
    countSet(IF,IF_count), countSet(IC,IC_count), % Compta el nombre de pistes de cada fila i de cada columna respectivament
    max_list(IF_count,IF_space), max_list(IC_count, IC_space), % Mira quin es el conjunt de pistes més gran a les files i a les columnes
    preprocessHints(IC_space,IC_count,IC,IC_processed), % Preprocessa les pistes de les columnes, posant espais perquè quedi ben imprès
    transpose(IC_processed, IC_printable), % proprocessIC retorna les pistes de columnes per columna. Transposa a files per imprimir com s'espera
    print('\n'), % Imprimeix un salt de línia (en cas de tenir més d'una solució, en prèmer el caràcter 'a' no es produeix un salt de línia automàtic)
    printHead(2*IF_space-1,IC_printable), % Imprimeix els espais, el separador vertical, i les pistes de columna.
    Aux is 2*IF_space-1, printMult('-',Aux), print('-'), printMult('-',2*NC-1), print('\n'), % Imprimeix el separador horitzontal
    preprocessHints(IF_space,IF_count,IF,IF_printable), % Preprocessa les pistes de les files, posant espais perquè quedi ben imprès
    printBody(IF_printable,G), !. % Imprimeix el cos i para quan troba la primera solució

% Preprocessa un conjunt de conjunts de pistes IC (amb subconjunts de llargada NC_count) amb un màxim d'espai Space. Res és el resultat
preprocessHints(_,[],[],[]).
preprocessHints(Space,[This_count|NC_count],[This_IC|IC],[R|Res]) :- 
    reverse(This_IC,This_IC_aux), % Gira l'ordre de les pistes (prerrequisit de processColumn)
    processColumn(Space,This_count,This_IC_aux,RAux), % Processa
    reverse(RAux,R), % Gira l'ordre de les pistes AMB els espais (post requisit de processColumn)
    preprocessHints(Space,NC_count,IC,Res). % Preprocessa la següent

% Retorna Column de mida Size, amb el contingut de Hints (de llargada Length) concatenat d'espais
processColumn(0,0,[],[]).
processColumn(Size,0,[],[I|Column]) :- Size > 0, I = ' ', NextSize is Size - 1, processColumn(NextSize,0,[],Column).
processColumn(Size, Length, [H|Hints], [I|Column]) :- Length > 0, I is H, NextSize is Size - 1, NextLength is Length - 1, processColumn(NextSize, NextLength, Hints, Column).

% Imprimeix les pistes de columnes R deixant InitSpace a la esquerra
printHead(_,[]).
printHead(InitSpace,[R|Rows]) :- printMult(' ',InitSpace), print('|'), printRow(R), print('\n'), printHead(InitSpace,Rows).
    % Espai, separador, pista, següent fila

% I és una llista de n llistes, R és el resultat (una llista amb n llistes on R[n] és la llargada de I[n])
countSet([],[]).
countSet([I|Input],[R|Result]) :- length(I,R), countSet(Input,Result).

% Imprimeix N cops C
printMult(_,-1).
printMult(_,0).
printMult(C,N) :- N > 0, print(C), Nnext is N - 1, printMult(C,Nnext).

% Imprimeix les pistes H de files i la matriu G línia per línia
printBody([],[]).
printBody([H|Hints],[R|G]) :- printRow(H), print('|'), printRow(R), print('\n'), printBody(Hints,G). % Pistes, separador, caselles, següent fila

% Imprimeix una fila X posant espais entre caràcters
printRow([]).
printRow([X|R]) :- print(X), printSeparator(R), printRow(R).

% Imprimeix un espai si queden caràcters, altrament no imprimeix res
printSeparator([]).
printSeparator(_) :- print(' ').
% ======================================================================================================