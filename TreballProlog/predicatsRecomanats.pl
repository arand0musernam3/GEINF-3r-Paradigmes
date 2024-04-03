%== Per no ser repetitiu, a les descripcions es parla sempre de fila, pero el mateix val per les columnes ==

%rowDef(Hints,Ts,+Len) : Hints es la llista d'indicacions per la fila Ts, que te llargada Len. ¡¡ Ni NHints ni Ts cal que estiguin instanciats !!
% 	- Si nomes feu el solucionador: rowDef(+Hints,Ts,+Len): les indicacions de la fila venen donades
%   - Si nomes feu el generador: rowDef(Hints,+Ts,+Len): el contingut de la fila ja ve donat

%cas base
rowDefSolver([], [], 0).

%només queda una pista, cal emplenar fins al final
rowDefSolver([H|[]], Ts, Len):- Len>=H, listOf('x', AuxVectX, H), AuxLen is Len - H,
                            listOf(' ', AuxVect0, AuxLen), append(AuxVectX, AuxVect0, Ts).

%queden més pistes, primera vegada que arribes aquí
rowDefSolver([H|Hints],Ts,Len):- Len>=H, length(Hints, Aux), Aux > 0, listOf('x', AuxVectH, H), append(AuxVectH, [' '], AuxVectH0), 
                            AuxLen is Len - H - 1, rowDefSolver(Hints, AuxVec, AuxLen), append(AuxVectH0, AuxVec, Ts).

%afegir un 0 al davant i queden més pistes
rowDefSolver([H|Hints],Ts,Len):- Len>=H, AuxLen is Len - 1, length([H|Hints], Aux), Aux > 0,
                            rowDefSolver([H|Hints],AuxVec,AuxLen), append([' '], AuxVec, Ts).


rowDefGenerator([],[],0).

rowDefGenerator(Res, Ts, Len):-
    length(Ts,Len),
    countSublists(Ts,[],Res).

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





%[x, 0, x, x, 0], [x, 0, 0, x, x]

%rowDefNFullNHints(Hints,Ts,+Len,+NFull,+NHints) : Hints es la llista de llargada NHints. Hints son les indicacions de la fila Ts, que te llargada Len.
% Podeu obviar aquest predicat, o assumir +Hints o +Ts si feu el solucionador o generador respectivament.


%makeSublists(X,+Xs,+N) : Xs es una llista de llistes que contenen nomes X. El recompte total delements a Xs es N. 
% llargada(Xs) esta instanciat, pero les seves subllistes no cal que ho estiguin.
% Exemple: length(XS,2),makeSublists('a',XS,3). 
% XS = [[],['a','a','a']] ? ;
% XS = [['a'],['a','a']] ? ;
% XS = [['a','a'],['a']] ? ;
% XS = [['a','a','a'],[]] ? ;
% no


makeSublists(X, Xs, N):- length(Xs,Len), generateCombinationsList(Len,N,R), iMakeSublists(X,R,Xs).

% Predicate to generate a list of non-negative integers that sums up to Sum
sumHintsZero([], 0).
sumHintsZero([X|Xs], Sum) :-
    between(0, Sum, X),
    NewSum is Sum - X,
    sumHintsZero(Xs, NewSum).

% Predicate to generate all combinations of lists that sum up to Sum
generateCombinationsList(N, Sum, Combinations) :-
    length(Combinations, N),       % Ensure Combinations has length N
    sumHintsZero(Combinations, Sum).  % Generate list of length N with sum equal to Sum

iMakeSublists(_,[],[]).
iMakeSublists(Caract, [N|Ns], [List|Lists]):-
    iMakeSublist(Caract, N, List),
    iMakeSublists(Caract, Ns, Lists).

iMakeSublist(_,0,[]).
iMakeSublist(Caract,N,[Caract|Ns]):-
    N>0,
    AuxLen is N - 1,
    iMakeSublist(Caract, AuxLen,Ns).

%mergeIntercal(+Xs,+Ys,Zs) : llargada(Xs)=llargada(Ys)+1, i Zs es el resultat d'intercalar un element de cada llista, comencant per Xs
%Exemple: mergeIntercal([a,a,a],[b,b],Zs).
% Zs=[[a,b,a,b,a]] 

mergeIntercal([X],[],[X]).
mergeIntercal([X|Xs],[Y|Ys],[X,Y|Rs]):- mergeIntercal(Xs,Ys,Rs).

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

%sumHints(+L,+N): Els elements de la llista L (llista d'enters) sumen N. 
% N esta instanciat, i llargada(L) esta instanciada, pero els valors de L no cal que estiguin instanciats.
% Els valors de L van de 1 a N.
%Exemple: sumHints([X,Y],4).
%X = 1
%Y = 3 ? ;
%X = 2
%Y = 2 ? ;
%X = 3
%Y = 1 ? ;
%no

sumHints([], 0).
sumHints([X|Xs], Y):- between(1, Y, X), Remaining is Y - X, sumHints(Xs, Remaining).


%listOf(X,L,+N): L es una llista que conte N ocurrencies de X
listOf(_, [], 0).
listOf(Caract, [Caract|Res], Len):- Len > 0, AuxLen is Len-1, listOf(Caract, Res, AuxLen). 

%Jocs de proves (feu-ne mes vosaltres!)

%solucionarNono(2,2,[[2],[1]],[[2],[1]],G).
%generarNono(2,2,IF,IC,[['x','x'],['x',' ']]).
%solucionarNonoPrint(2,2,[[2],[1]],[[2],[1]],_).

%solucionarNono(3,3,[[1,1],[1],[1,1]],[[1,1],[1],[1,1]],G).
%generarNono(3,3,IF,IC,[['x',' ','x'],[' ','x',' '],['x',' ','x']]).
%generarNonoPrint(3,3,_,_,[['x',' ','x'],[' ','x',' '],['x',' ','x']]).

%solucionarNono(5,5,[[1,2],[2,2],[1],[3,1],[1]],[[4],[1,1],[2],[2],[2,1]],G).
%generarNono(5,5,IF,IC,[['x',' ',' ','x','x'],['x','x',' ','x','x'],['x',' ',' ',' ',' '],['x','x','x',' ','x'],[' ',' ','x',' ',' ']]).

%nonoBidirec(5,5,[IF1,[2,2],IF3,[3,IF4_2],[1]],[[4],[1,1],[2],[2],[2,1]],[['x',C22,' ','x','x'],['x','x',' ','x','x'],['x',' ',' ',' ',' '],['x','x','x',' ','x'],F5]).

%nonoBidirecPrint(5,5,[_,[2,2],_,[3,_],[1]],[[4],[1,1],[2],[2],[2,1]],	[['x',_,' ','x','x'],['x','x',' ','x','x'],['x',' ',' ',' ',' '],['x','x','x',' ','x'],_]).


% ============================================ SOLUCIONADOR ============================================

%retorna totes les possibles combinacions de files per una matriu
totesCombinacionsFiles(Len,[], []).

totesCombinacionsFiles(Len, [F|Fs],[Res|Resta]):-
    findall(X, rowDefSolver(F,X,Len), Res),
    totesCombinacionsFiles(Len,Fs,Resta).


%1r param llista de llistes, segon param llista de llista que són totes les combinacions possibles
% cas base, si només hi ha una llista retornem tots els elements de la llista
generarTotesCombinacions([List], Combinations) :-
    findall([X], member(X, List), Combinations).

%cas recursiu, per cada llista generem les combinacions amb els elements de les altres llistes.
generarTotesCombinacions([List1, List2|T], Combinations) :-
    generarTotesCombinacions([List2|T], Combinations2),
    findall([X|Y], (member(X, List1), member(Y, Combinations2)), Combinations).


%donada una llista de files i una llista de pistes, comprova si les files compleixen les pistes
comprovarFiles(_,[],[]).

comprovarFiles(Len, [Pista|Pistes],[Fila|LlistaFiles]):-
    rowDefSolver(Pista,Fila,Len),
    comprovarFiles(Len,Pistes,LlistaFiles).

testejarMatrius(_,_,_,_,[],_):-fail.

testejarMatrius(NF,NC,IF,IC,[Mat|LlistaMatrius],Mat):-
    transpose(Mat, Columnes),
    comprovarFiles(NC,IF,Mat),
    comprovarFiles(NF,IC,Columnes).

testejarMatrius(NF,NC,IF,IC,[Mat|LlistaMatrius],G):-
    testejarMatrius(NF,NC,IF,IC,LlistaMatrius,G).


solucionarNono(NF,NC,IF,IC,G):-
    totesCombinacionsFiles(NC, IF, TotesCombinacionsFiles),
    generarTotesCombinacions(TotesCombinacionsFiles,TotesMatriusPossibles),
    testejarMatrius(NF,NC,IF,IC,TotesMatriusPossibles,G).

% ======================================================================================================

% ============================================= GENERADOR ==============================================
generarFila(_,[],[]).

generarFila(Len, [Pista|Pistes],[Fila|Files]):-
    rowDefGenerator(Pista, Fila, Len),
    generarFila(Len, Pistes, Files).

generarNono(NF,NC,IF,IC,G):-
    transpose(G,Columnes),
    generarFila(NC,IF,G),
    generarFila(NF,IC,Columnes).
% ======================================================================================================