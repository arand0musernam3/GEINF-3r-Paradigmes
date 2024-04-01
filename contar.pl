% count(+L,+E,N), E apareix exactament N cops a L.
count([],_,0).
count([X|Xs],X,N) :- count(Xs,X,Np), N is Np+1.
count([Y|Xs],X,N) :- X\=Y, count(Xs,X,N).

%Aquesta implementació és correcte quan preguntem quants cops apareix un element 
%concret en una llista concreta.

%Però si preguntem quin (o quins) elements apareixen exactament N cops 
%en una llista concreta no va bé. (és a dir si volem que la E pugui ser una variable)

%| ?- count([1,1,2],E,1).    

%no

%Com pot ser? el problema és que es troba amb que 1 apareix 2 cops i al fer 
%backtraking per poder "anar" per la darrera regla, cal satisfer la condició X\=Y, 
%que significa que X no es pot unificar amb Y, i com podem deduir això no es 
%satisfà perquè si que es pot unificar en el nostre cas E amb 1, 
%per tant ja no avança i no pot trobar que el 2 apareix un sol cop. Per arreglar 
%aquest problema hem de canviar l'ordre. 

% countprima(+L,E,N), E apareix exactament N cops a L.
countprima([],_,0).
countprima([X|Xs],X,N) :- countprima(Xs,X,Np), N is Np+1.
countprima([Y|Xs],X,N) :-  countprima(Xs,X,N), X\=Y.
