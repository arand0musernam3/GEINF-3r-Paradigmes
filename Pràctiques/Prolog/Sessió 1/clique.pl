% DEFINICIO DEL GRAF
arest(n1,n2).
arest(n1,n4).
arest(n1,n6).

arest(n2,n1).
arest(n2,n3).
arest(n2,n4).
arest(n2,n6).

arest(n3,n2).
arest(n3,n4).

arest(n4,n1).
arest(n4,n2).
arest(n4,n3).
arest(n4,n5).
arest(n4,n6).

arest(n5,n4).
arest(n5,n6).

arest(n6,n1).
arest(n6,n2).
arest(n6,n4).
arest(n6,n5).

% DEFINICIONS DE PRIORITATS
ordre(n1,n2).
ordre(n1,n3).
ordre(n1,n4).
ordre(n1,n5).
ordre(n1,n6).

ordre(n2,n3).
ordre(n2,n4).
ordre(n2,n5).
ordre(n2,n6).

ordre(n3,n4).
ordre(n3,n5).
ordre(n3,n6).

ordre(n4,n5).
ordre(n4,n6).

ordre(n5,n6).


% CLIQUE3 (pocho)
tots3(A,B,C) :- arest(A,B), arest(A,C).
clique3(A,B,C) :- tots3(A,B,C), tots3(B,A,C), tots3(C,A,B), ordre(A,B), ordre(A,C), ordre(B,C).

% CLIQUE4 (pocho)
tots4(A,B,C,D) :- arest(A,B), arest(A,C), arest(A,D).
clique4(A,B,C,D) :- tots4(A,B,C,D), tots4(B,C,D,A), tots4(C,D,A,B), tots4(D,A,B,C), ordre(A,B), ordre(A,C), ordre(A,D), ordre(B,C), ordre(B,D), ordre(C,D).