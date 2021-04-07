% Tenemos 6 personas sentadas en una mesa. Represente con el predicado binario izquierda_de(X,Y).
% Defina los siguietnes predicados: derecha_de, vecinos, igual_vecino.

izquierda_de(pedro, ana).
izquierda_de(ana, luis).
izquierda_de(luis, antonio).
izquierda_de(antonio, eva).
izquierda_de(eva, sofia).
izquierda_de(sofia, pedro).

derecha_de(X,Y):- izquierda_de(Y,X).

vecinos(X,Y):- izquierda_de(X,Y).
vecinos(X,Y):- izquierda_de(Y,X).

igual_vecino(X,Y):- izquierda_de(X,Z), izquierda_de(Z,Y).
% tambien se puede hacer con vecinos
