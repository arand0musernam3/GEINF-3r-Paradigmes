%%% PROGRAMES QUE PODEM UTILITZAR %%%
%%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%% %%%

% removeOne(X,L1,L2) => L2 es L1 amb una ocurrència menys de X.
remove_one(_,[],[]).
remove_one(X,[X|L1],L1).
remove_one(X,[Y|L1],[Y|L2]) :- X=Y,remove_one(X,L1,L2).

% count(L,X,N) => N es el nombre de vegades que X apareix a L.
count([],_,0).
count([X|Xs],X,N) :- count(Xs,X,Np), N is Np+1.
count([Y|Xs],X,N) :- X\=Y, count(Xs,X,N).

% split(X,L,LEQ,GT) => LEQ son els elements de L menors o iguals que X, GT son els mes grans que X
split(_,[],[],[]).
split(X,[Y|L],[Y|LEQs],GT) :- Y=<X, split(X,L,LEQs,GT).
split(X,[Y|L],LEQ,[Y|GTs]) :- Y>X, split(X,L,LEQ,GTs).

% nessim(L,N,X) => X apareix a la posicio N de L.
nessim([X|_],0,X).
nessim([_|Xs],N,X) :- N>0,
                      Np is N-1,
                      nessim(Xs,Np,X).

% quicksort(L1,L2) => L2 es la llista L1 ordenada.
quicksort([],[]).
quicksort([X|Xs],L) :- split(X,Xs,LEQ,GT),
                       quicksort(LEQ,LEQsort),
                       quicksort(GT,GTsort),
                       append(LEQsort,[X|GTsort],L).

%%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%% %%%

% sublist_(Lp, L) => Lp és subllista de L
% una subllista és el sufix del prefix.
sublist_(A,B) :- prefix(X,B), suffix(A,X).

% palindrome_(L)
palindrome_(L) :- reverse(Y,L), Y=L.

% insert_(X, Xs, Ys) => Ys es el resultat d'inserir ordenadament X a Xs, assumint que Xs esta en ordre ascendent
insert_(X, Xs, Ys) :- split(X, Xs, LEQ, GT), append(LEQ, [X], LEQX), append(LEQX, GT, R), R = Ys.

% sortinsert_(L, Ls) => Ls es la llista L ordenada. Feu servir insert
sort_insert_([],[]).
sort_insert_([X|Xs], Ls) :- sort_insert_(Xs, Temp), insert_(X, Temp, Ls).

% union_(Xs, Ys, Zs) => Zs = Xs unio Ys
union_(Xs, Ys, Zs) :- append(Xs,Ys,R), quicksort(R, Zs). %ESTÀ MALAMENT CAGAMOS

% inter(Xs, Ys, Zs) => Zs = Xs interseccio Ys
inter([], _, []).

inter([H1|T1], L2, [H1|Res]) :-
    member(H1, L2),
    remove_one(H1, L2, Aux),
    inter(T1, Aux, Res).

inter([_|T1], L2, Res) :-
    inter(T1, L2, Res).

% difference_(Xs, Ys, Zs) => Zs = Xs \ Ys
% difference_(Xs, Ys, Zs) :- .

% multiset_to_set_(Xs, Zs) => Zs is Xs sense repeticions
multiset_to_set_([],[]).
multiset_to_set_(Xs, Zs) :- sort(Xs, Zs).

% sum_(L,N) => N es la suma dels elements de L
sum_([],0).
sum_([L1|L], N) :- sum_(L,R), N is R + L1.

% sum_even_(L,N) => N es la suma dels nombres parells de L
sum_even_([],0).
sum_even_([L1|L], N) :- sum_even_(L,R), 0 is L1 mod 2, N is R + L1.
sum_even_([L1|L], N) :- sum_even_(L,R), 1 is L1 mod 2, N is R + 0. % preguntar perquè sense el "1 is L1 mod 2" no va bé la cosa :P

% gcd_(A, B, M) => M es el maxim comu divisor de A i B
gcd_(N1, N2, R) :- N1 = N2, R = N1. 
gcd_(N1, N2, R) :-
    N1 < N2,
    Aux is N2 - N1,
    gcd_(N1, Aux, R).
gcd_(N1, N2, R) :-
    N1 > N2,
    gcd_(N2,N1,R).

% paths(E,X,Y,P) 
% E son les arestes d'un graf dirigit aciclic.
% La llista P conte un cami de X a Y.
paths(E,X,Y,P) :- member(ar(X,Y),E), P=[X,Y].
paths(E,X,Y,P) :- member(ar(X,Z),E), paths(E,Z,Y, Aux), P=[X|Aux].


% clique(g(V,E),C) 
% V,E son els nodes i arestes d'un graf. 
% Les arestes son parelles ordenades de nodes ar(N1,N2) tal que N1 < N2
% La llista C conte un clique ordenat del graf g(V,E).
clique(g(V,E),C) :- .

%Exemples d'execucio

%clique(g([1,2,3],[ar(1,2),ar(1,3),ar(2,3)],C).
%C = [1] ? ;
%C = [2] ? ;
%C = [3] ? ;
%C = [1,2] ? ;
%C = [1,3] ? ;
%C = [2,3] ? ;
%C = [1,2,3] ? ;

%Graf de la sessio 1
%findall(C,clique(g([1,2,3,4,5,6],[ar(1,2),ar(1,4),ar(1,6),ar(2,3),ar(2,4),ar(2,6),ar(3,4),ar(4,5),ar(4,6),ar(5,6)]),C),L).
% L = [[1],[2],[3],[4],[5],[6],[1,2],[1,4],[1,6],[2,3],[2,4],[2,6],[3,4],[4,5],[4,6],[5,6],[1,2,4],[1,2,6],[1,4,6],[2,3,4],[2,4,6],[4,5,6],[1,2,4,6]]