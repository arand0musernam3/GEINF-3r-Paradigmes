/*
A qui li agrada en John?
    loves(X,john).
    - Laura
Qui li agrada a l'Ann?
    loves(ann,X).
    - Michael
Qui està enamorat d'algú?
    - loves(X,_).
    - John, ann, luis, michael, laura, isabel
Qui és estimat per algú?
    - loves(_,X).
    - ann, michael, isabel, ann, john, luis
Quines dues persones s'estimen mútuament?
    - loves(X,Y),loves(Y,X).
    - ann & michael, luis & isabel
Qui estima sense ser correspost? Nota: podeu fer servir la negació amb \+ loves(...) 
    - loves(X,Y),\+loves(Y,X).
    - john i laura.
*/

loves(john, ann).
loves(ann, michael).
loves(luis, isabel).
loves(michael, ann).
loves(laura, john).
loves(isabel, luis).
