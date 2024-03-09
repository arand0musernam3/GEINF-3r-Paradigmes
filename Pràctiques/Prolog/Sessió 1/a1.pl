  parent(alice,bob).
  parent(bob,eve).
  ancestor(X,Y):-parent(X,Y).
  ancestor(X,Y):-parent(X,Z),ancestor(Z,Y).

  /* acaba perfectament */