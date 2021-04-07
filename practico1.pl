%  Ejercicio 1

progenitor(marta, andres).
progenitor(marta, franco).
progenitor(iaia, leonardo).
progenitor(iaia, pedro).
progenitor(ana, marta).
progenitor(juan, jose).
progenitor(jose, pedro).
progenitor(pedro, maria).
casados(marta, leonardo).
casados(pedro, gabriela).

hermano(X, Y) :- progenitor(Z,X), progenitor(Z,Y), distintos(X,Y).

tio(X, Y) :- progenitor(Z,Y), hermano(X,Z).

tio_politico(X, Y) :- casados(X, Z), tio(Z, Y).

cunado(X, Y) :- hermano(X, Z), casados(Y, Z).

concunado(X, Y) :- hermano(X, Z), casados(Z, Q), hermano(Q, Y).

suegro(X, Y) :- progenitor(X, Z), casados(Y, Z).

consuegro(X, Y) :- casados(Z, Q), progenitor(X, Z), progenitor(Y, Q).

% Ejercicio 2

% Si 1 2 4 5 6 8 10 11 12 14
% No 3 7 9 13

% Ejercicio 3

% suma(X,Y,Z) <- Z es el resultado
suma(0, X, X).
suma(s(X), Y, s(Z)) :- suma(X, Y, Z).

resta(0, 0, 0).
resta(X, Y, R) :- suma(Y, R, X).

producto(s(0), Y, Y).
producto(s(X), Y, P) :- producto(X, Y, S), suma(S, Y, P).

distintos(X, Y) :- resta(X, Y, s(_)).
distintos(X, Y) :- resta(Y, X, s(_)).

mayor(s(_), 0).
mayor(s(X), s(Y)) :- mayor(X, Y).

factorial(0,s(0)).
factorial(s(X), R) :- factorial(X, S), producto(S, s(X), R).

potencia(_, 0, s(0)).
potencia(X, s(Y), R) :- potencia(X, Y, S), producto(S, X, R).

% Ejercicio 4

% a)
largo([], 0).
largo([_|L], s(N)) :- largo(L, N).

ultimo([X], X).
ultimo([_|L], X) :- ultimo(L, X).

sin_ultimo([_], []).
sin_ultimo([X|L], [X|S]) :- sin_ultimo(L, S).

naiveReverse([], []).
naiveReverse([H|T], R) :- naiveReverse(T, RT), append(RT, [H], R).

reverseAc([], Ac, Ac).
reverseAc([H|T], Ac, Rev) :- reverseAc(T, [H|Ac], Rev).

reverse(L1, L2) :- reverseAc(L1, [], L2).

subsecuencia([],[]).
subsecuencia([X|L], [X|S]) :- subsecuencia(L, S).
subsecuencia([_|L], S) :- subsecuencia(L, S).

sublista(L, S) :- append(_, S, L), append(S, _, L).

prefijo(L, Pref) :- append(Pref, _, L).

sufijo(L, Suf) :- append(_, Suf, L).

borrar_todos([], _, []).
borrar_todos([X|L], X, B) :- borrar_todos(L, X, B).
borrar_todos([Y|L], X, [Y|B]) :- borrar_todos(L, X, B), distintos(Y, X).

sin_repetidos([], []).
sin_repetidos([X|L], [X|S]) :- borrar_todos(L, X, B), sin_repetidos(B, S).

% b)

conjunto(C) :- sin_repetidos(C, C).
son_conjuntos(A, B) :- conjunto(A), conjunto(B).

esta_en(X, [X|_]). 
esta_en(X, [Y|L]) :- distintos(X,Y), esta_en(X, L).

no_esta_en(_, []).
no_esta_en(X, [Y|L]) :- distintos(X,Y), no_esta_en(X, L).

conj_iguales(X,X).
conj_iguales([X|C1], C2) :- son_conjuntos([X|C1],C2), esta_en(X, C2), borrar_todos(C2, X, R), conj_iguales(C1, R).

subconjunto(_, []).
subconjunto(C, [X|S]) :- son_conjuntos(C, [X|S]), esta_en(X, C), subconjunto(C, S).

interseccion([], _, []).
interseccion([X|C1], C2, [X|I]) :- son_conjuntos([X|C1],C2), esta_en(X, C2), interseccion(C1, C2, I).
interseccion([X|C1], C2, I) :- son_conjuntos([X|C1],C2), no_esta_en(X, C2), interseccion(C1, C2, I).

union([], C, C). 
union([X|C1], C2, U) :-  son_conjuntos([X|C1],C2), esta_en(X, C2), union(C1, C2, U).
union([X|C1], C2, [X|U]) :-  son_conjuntos([X|C1],C2), no_esta_en(X, C2), union(C1, C2, U).

% Ejercicio 5

sumaLista([], 0).
sumaLista([0|Ns], N) :- sumaLista(Ns, N).
sumaLista([s(X)|Ns], s(N)) :- sumaLista([X|Ns], N).

% Ejercicio 6

ancestro(X, Y, [X,Y]) :- progenitor(X, Y).
ancestro(X, Y, [X|L]) :- progenitor(X, P), ancestro(P, Y, L).

% Ejercicio 7

arbol_bin(vacio).
arbol_bin(arbol(_, Izq, Der)) :- arbol_bin(Izq), arbol_bin(Der).

member_ab(X, arbol_bin(arbol(X, _, _))).
member_ab(X, arbol_bin(arbol(_, Izq, _))) :- member_ab(X, Izq).
member_ab(X, arbol_bin(arbol(_, _, Der))) :- member_ab(X, Der).

% member_ab(s(0), arbol_bin(arbol(s(0),arbol(s(0),vacio,vacio),arbol(s(0),vacio,vacio))).)

cambiar(_, arbol_bin(vacio), arbol_bin(vacio)).
cambiar(X, Y, arbol_bin(arbol(X, AI1, AD1)), arbol_bin(arbol(Y, AI2, AD2))) :- cambiar(X, Y, AI1, AI2), cambiar(X, Y, AD1, AD2).

sumar(arbol_bin(arbol(X, vacio, vacio)), X).
sumar(arbol_bin(arbol(X, I, D)), R) :- sumar(arbol_bin(I), RI), sumar(arbol_bin(D), RD), suma(RI, RD, RR), suma(RR, X, R).

abb(arbol_bin(arbol(_, vacio, vacio))).
abb(arbol_bin(arbol(X, arbol(Y, I1, D1), arbol(Z, I2, D2)))) :- mayor(X, Y), mayor(Z, X), abb(arbol_bin(arbol(Y, I1, D1))), abb(arbol_bin(arbol(Z, I2, D2))).

% Ejercicio 8
sigma([a,b,c]).

or(_, _).
concat(_,_).
kleen(_).

exp_reg(_, 0).
exp_reg(_, e).
exp_reg(S, X) :- sigma(S), member(X, S).
exp_reg(S, or(R1, R2)) :- sigma(S), exp_reg(S, R1), exp_reg(S, R2).
exp_reg(S, concat(R1,R2)) :- sigma(S), exp_reg(S, R1), exp_reg(S, R2).
exp_reg(S, kleen(R)) :- exp_reg(S, R).

pertenece(_, 0, []).

pertenece(_, e, [e]).

pertenece(S, X, [X]) :- sigma(S), member(X, S).

pertenece(S, or(R1, R2), X) :- exp_reg(S, or(R1, R2)), pertenece(S, R1, X).
pertenece(S, or(R1, R2), X) :- exp_reg(S, or(R1, R2)), pertenece(S, R2, X).

pertenece(S, concat(R1, R2), X) :- exp_reg(S, concat(R1, R2)), pertenece(S, R1, P1), pertenece(S, R2, P2), append(P1, P2, X).

pertenece(S, kleen(R), X) :- exp_reg(S, kleen(R)), pertenece(S, R, P1), pertenece(S, kleen(R), P2), append(P1, P2, X).

