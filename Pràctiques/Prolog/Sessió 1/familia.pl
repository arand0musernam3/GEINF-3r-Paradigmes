father(tom,jack). %f1
father(tom,lisa). %f2
father(will,tom). %f3
mother(ann,tom). %f4
mother(ann,john). %f5
parent(X1,Y1):-father(X1,Y1). %r1
parent(X2,Y2):-mother(X2,Y2). %r2
grandfather(X3,Y3):-father(X3,Z3),parent(Z3,Y3). %r3
siblings(X4,Y4):-parent(Z4,X4),parent(Z4,Y4),X4\=Y4. %r4

/*

*/