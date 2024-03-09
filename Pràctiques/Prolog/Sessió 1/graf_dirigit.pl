% DEFINICIÓ DEL GRAF
arest(n1,n2).
arest(n1,n3).
arest(n2,n5).
arest(n3,n5).
arest(n3,n6).
arest(n4,n3).
arest(n4,n7).
arest(n5,n6).

% DEFINICIÓ DE LA FUNCIÓ CAMÍ
cami(X,Y) :- arest(X,Y).
cami(X,Y) :- arest(X,Z), cami(Z,Y). %literalment com el parentesc
