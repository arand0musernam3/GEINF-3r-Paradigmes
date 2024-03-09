  parent(alice,bob).
  parent(bob,eve).
  ancestor(X,Y):-parent(X,Z),ancestor(Z,Y).
  ancestor(X,Y):-parent(X,Y).

  /*tutto benne*/