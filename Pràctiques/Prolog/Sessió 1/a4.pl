  parent(alice,bob).
  parent(bob,eve).
  ancestor(X,Y):-ancestor(Z,Y),parent(X,Z).
  ancestor(X,Y):-parent(X,Y).

  /* stack overflow i no dona cap resultat */