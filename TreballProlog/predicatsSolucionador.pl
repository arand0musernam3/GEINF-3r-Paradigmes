[predicatsRecomanats].
solucionarNono(NF,NC,IF,IC,G):-
    totesCombinacionsFiles(NC, IF, TotesCombinacionsFiles),
    generarPossiblesMatriusAPartirFiles(TotesCombinacionsFiles,PossiblesMatrius).