  parent(alice,bob).
  parent(bob,eve).
  ancestor(X,Y):-parent(X,Y).
  ancestor(X,Y):-ancestor(Z,Y),parent(X,Z).
/* stack overflow baby pero dona solucions */