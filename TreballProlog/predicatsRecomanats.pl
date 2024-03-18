%== Per no ser repetitiu, a les descripcions es parla sempre de fila, pero el mateix val per les columnes ==

%rowDef(Hints,Ts,+Len) : Hints es la llista d'indicacions per la fila Ts, que te llargada Len. ¡¡ Ni NHints ni Ts cal que estiguin instanciats !!
% 	- Si nomes feu el solucionador: rowDef(+Hints,Ts,+Len): les indicacions de la fila venen donades
%   - Si nomes feu el generador: rowDef(Hints,+Ts,+Len): el contingut de la fila ja ve donat

%rowDefNFullNHints(Hints,Ts,+Len,+NFull,+NHints) : Hints es la llista de llargada NHints. Hints son les indicacions de la fila Ts, que te llargada Len.
% Podeu obviar aquest predicat, o assumir +Hints o +Ts si feu el solucionador o generador respectivament.


%makeSublists(X,+Xs,+N) : Xs es una llista de llistes que contenen nomes X. El recompte total delements a Xs es N. 
% llargada(Xs) esta instanciat, pero les seves subllistes no cal que ho estiguin.
% Exemple: length(XS,2),makeSublists('a',XS,3). 
% XS = [[],['a','a','a']] ? ;
% XS = [['a'],['a','a']] ? ;
% XS = [['a','a'],['a']] ? ;
% XS = [['a','a','a'],[]] ? ;
% no

%mergeIntercal(+Xs,+Ys,Zs) : llargada(Xs)=llargada(Ys)+1, i Zs es el resultat d'intercalar un element de cada llista, comencant per Xs
%Exemple: mergeIntercal([a,a,a],[b,b],Zs).
% Zs=[[a,b,a,b,a]] 

%transpose(+Xs,Ys): Ys es la matriu (llista de llistes) Xs transposada. 
% Assumim que es una matriu ben construida, es a dir, totes les files (subllistes) tenen la mateixa llargada.


%sumHints(+L,+N): Els elements de la llista L (llista d'enters) sumen N. 
% N esta instanciat, i llargada(L) esta instanciada, pero els valors de L no cal que estiguin instanciats.
% Els valors de L van de 1 a N.
%Exemple: sumHints([X,Y],4).
%X = 1
%Y = 3 ? ;
%X = 2
%Y = 2 ? ;
%X = 3
%Y = 1 ? ;
%no
sumHints([], 0).
sumHints([X|Xs],Y):- sumHints(Xs, R), Y is R+X. %va bé per comprovar però no per generar

%listOf(X,L,+N): L es una llista que conte N ocurrencies de X

%Jocs de proves (feu-ne mes vosaltres!)

%solucionarNono(2,2,[[2],[1]],[[2],[1]],G).
%generarNono(2,2,IF,IC,[['x','x'],['x',' ']]).
%solucionarNonoPrint(2,2,[[2],[1]],[[2],[1]],_).

%solucionarNono(3,3,[[1,1],[1],[1,1]],[[1,1],[1],[1,1]],G).
%generarNono(3,3,IF,IC,[['x',' ','x'],[' ','x',' '],['x',' ','x']]).
%generarNonoPrint(3,3,_,_,[['x',' ','x'],[' ','x',' '],['x',' ','x']]).

%solucionarNono(5,5,[[1,2],[2,2],[1],[3,1],[1]],[[4],[1,1],[2],[2],[2,1]],G).
%generarNono(5,5,IF,IC,[['x',' ',' ','x','x'],['x','x',' ','x','x'],['x',' ',' ',' ',' '],['x','x','x',' ','x'],[' ',' ','x',' ',' ']]).

%nonoBidirec(5,5,[IF1,[2,2],IF3,[3,IF4_2],[1]],[[4],[1,1],[2],[2],[2,1]],[['x',C22,' ','x','x'],['x','x',' ','x','x'],['x',' ',' ',' ',' '],['x','x','x',' ','x'],F5]).

%nonoBidirecPrint(5,5,[_,[2,2],_,[3,_],[1]],[[4],[1,1],[2],[2],[2,1]],	[['x',_,' ','x','x'],['x','x',' ','x','x'],['x',' ',' ',' ',' '],['x','x','x',' ','x'],_]).

