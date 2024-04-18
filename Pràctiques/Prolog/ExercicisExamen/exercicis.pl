% p(a).
% p(b).
% p(X):-p(X).
% q(X):-p(X),!,p(X).


% p(b).
% p(a):-p(b).
% p(X).

% canvia(LL,Old,New,LLN). tal que LLN sigui la llista LL on
% s’han canviat totes les ocurrències de la llista Old per la llista New.
% P.ex: canvia([1,2,3,1,2,4,1,2],[1,2],[3,3,3],[3,3,3,3,3,3,3,4,3,3,3]).
% Només pots utilitzar el predicat append. Posa el tall (NOMÉS) on
% calgui per tal que només ens doni una solució.
canvia([],_,_,[]).

canvia(LL,Old,New,Res):-
    append(Old,X,LL),  %generem vector X amb la resta del vector
    !, %tallem per evitar solucions extres
    canvia(X,Old,New,Y), %continuem canviant
    append(New,Y,Res). %acabem

canvia([First|LL], Old, New, [First|Res]):-
    canvia(LL,Old,New,Res).

%conta(LL,M,N) on N compta quantes vegades ocorre la llista M a LL.
conta([],_,0).

conta([First|LL],M,N):-
    append(M,_,[First|LL]), %si això és possible vol dir que és prefix
    !,
    conta(LL,M,Aux),
    N is Aux + 1.

conta([_|LL],M,N):-
    conta(LL,M,N).


%treu(X,L,Lp) Lp és la llista L on s'ha tret una ocurrència d'X.
treu(_,[],[]).

treu(X,[Y|L],[Y|Lp]):-
    X \= Y, 
    treu(X,L,Lp).

treu(X,[X|L],L).


% exercici del final de l'examen

remove(_,[],[]).
remove(X,[X|L1],L2) :- remove(X,L1,L2).
remove(X,[Y|L1],[Y|L2]) :- X \= Y, remove(X,L1,L2).

count(_,[],0).
count(X,[X|L],Aux):-
    count(X,L,N),
    Aux is N + 1.

count(X,[Y|L],N):-
    X \= Y,
    count(X,L,N).

multicojallista([],[]).
multicojallista([M|Mc],[(M,Nombre)|L]):-
    count(M,[M|Mc],Nombre),
    remove(M,[M|Mc],AuxVec),
    multicojallista(AuxVec, L).


arc(a,b).
arc(b,a).
arc(b,c).
cami(X,Y):- arc(X,Y).
cami(X,Z):- arc(X,Y), cami(Y,Z). %X==a, Z==c

apend([],Ys,Ys).
apend([X|Xs],Ys,[X|Zs]):- apend(Xs,Ys,Zs).

permutation([],[]).
permutation(L,[X|Xs]) :- append(V,[X|P],L),
                         append(V,P,W),
                         permutation(W,Xs).