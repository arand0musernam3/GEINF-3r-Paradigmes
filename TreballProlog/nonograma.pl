consult(predicatsRecomanats). % inclou el fitxer "predicatsRecomanats.pl"
consult(predicatsSolucionador). % inclou "predicatsSolucionador.pl"

/*
NF i NC: són el nombre de files i columnes respectivament, es passen instanciades.

IF i IC: són les indicacions de les files i les columnes respectivament. IF és una llista que
    conté un nombre NF de subllistes, cada subllista està instanciada, i la i-èssima
    fila és la llista d’enters corresponent a les indicacions de la fila i. El contingut de
    IC és anàleg per les indicacions de les columnes.

G: és una llista de llistes, no necessàriament instanciada, que conté la solució del
    nonograma fila per fila. Una casella buida s’indica com un caràcter espai en blanc
    ' ', i una casella pintada com un caràcter 'x'. La primera fila de la solució de la
    Figura 1 seria ['x','x',' ',' ','x'].
*/
% solucionarNono(+NF,+NC,+IF,+IC,G)
    
