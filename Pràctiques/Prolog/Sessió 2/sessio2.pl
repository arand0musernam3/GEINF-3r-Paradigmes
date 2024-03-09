%%% PROGRAMES QUE PODEM UTILITZAR %%%
%%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%% %%%

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
union_(Xs, Ys, Zs) :- append(Xs,Ys,R), quicksort(R, Zs). 

% intersection_(Xs, Ys, Zs) => Zs = Xs interseccio Ys
intersection([],_,[]).
intersection_([X|Xs], Ys, [X|Zs]) :- member(X, Ys), intersection_(Xs,Ys,Zs).
intersection_([_|Xs],Ys, Zs) :- intersection_(Xs,Ys,Zs).