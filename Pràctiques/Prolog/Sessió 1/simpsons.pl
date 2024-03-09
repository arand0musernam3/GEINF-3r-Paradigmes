
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% FETS

% Algunes relacions unaries.

nen(bart).
nen(milhouse).
nen(lisa).
nen(maggie).
nen(rod).
nen(todd).
nen(ralph).

home(abe).
home(homer).
home(bart).
home(ned).
home(rod).
home(todd).
home(chief_wiggum).
home(ralph).
home(milhouse).
home(mr_burns).
home(smithers).
home(groundskeeper_willie).
home(principal_skinner).

dona(marge).
dona(lisa).
dona(maggie).
dona(maude).
dona(mrs_krabappel).
dona(ms_hoover).
dona(patty).
dona(selma).
dona(jacqueline).

% Algunes relacions binaries:

% pare(X,Y) vol dir que X es pare de Y
pare(abe, homer).
pare(homer, bart).
pare(homer, lisa).
pare(homer, maggie).
pare(ned, rod).
pare(ned, todd).
pare(chief_wiggum,ralph).

% mare(X,Y) vol dir que X es mare de Y
mare(marge, bart).
mare(marge, lisa).
mare(marge, maggie).
mare(jacqueline, marge).
mare(jacqueline, patty).
mare(jacqueline, selma).
mare(maude, rod).
mare(maude, todd).

casat(homer, marge).
casat(ned, maude).

amic(bart, milhouse).
amic(homer, ned).
amic(marge, maude).

viu(homer, adr("Evergreen Terrace", 742, "Springfield")).
viu(ned, adr("Evergreen Terrace", 744, "Springfield")).


% REGLES


% X es progenitor de Y
progenitor(X,Y) :- pare(X,Y).
progenitor(X,Y) :- mare(X,Y).

% X i Y són germans
germa(X,Y) :- progenitor(Z,X), progenitor(Z,Y), X\=Y.

% ANCESTOR X es ancestor de Y
ancestor(X,Y) :- progenitor(X,Y).
ancestor(X,Y) :- progenitor(X,Z), ancestor(Z,Y).

% GRANDFATHER
grandfather(X,Y) :- pare(X, Z), progenitor(Z,Y).

% GRANDMOTHER
grandmother(X,Y) :- mare(X, Z), progenitor(Z,Y).

% UNCLE
uncle(X,Y) :- home(X), germa(X,Z), progenitor(Z,Y).
uncle(X,Y) :- home(X), casat(X,W), germa(W,Z), progenitor(Z,Y).

% AUNT
aunt(X,Y) :- dona(X), germa(X,Z), progenitor(Z,Y).
aunt(X,Y) :- dona(X), casat(X,W), germa(W,Z), progenitor(Z,Y).


/*
TODO:
  - Ancestor
  - Grandfather
  - Grandmother
  - Uncle
  - Aunt
 */