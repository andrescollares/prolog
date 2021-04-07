% Ejercicio 2

% a)

largo([], 0).
largo([_|L], N) :- largo(L, M), N is M + 1.

maximo([X], X).
maximo([X|L], X) :- maximo(L, M), X > M.
maximo([X|L], M) :- maximo(L, M), M >= X.

% b)

largo2(L, N) :- largoAc(L, 0, N).

largoAc([], X, X).
largoAc([_|L], Ac, N) :- Ac1 is Ac + 1, largoAc(L, Ac1, N).

maximo2(L, M) :- maximoAc(L, 0, M).

maximoAc([], X, X).
maximoAc([X|L], Ac, M) :- X > Ac, maximoAc(L, X, M).
maximoAc([X|L], Ac, M) :- X =< Ac, maximoAc(L, Ac, M).

% Ejercicio 3

suma([X], X).
suma([X|L], S) :- suma(L, R), S is R + X.

pares([], []).
pares([X|L], [X|P]) :- 0 is X mod 2, pares(L, P).
pares([X|L], P) :- 1 is X mod 2, pares(L, P).

mayores([], _, []).
mayores([X|L], Y, [X|M]) :- X > Y, mayores(L, Y, M).
mayores([X|L], Y, M) :- X =< Y, mayores(L, Y, M).

merge([], L2, L2).
merge(L1, [], L1).
merge([X|L1], [Y|L2], [X|L3]) :- X < Y, merge(L1, [Y|L2], L3).
merge([X|L1], [Y|L2], [Y|L3]) :- Y =< X, merge([X|L1], L2, L3).

% Ejercicio 4



% Ejercicio 5

neg([], []).
neg([X|V], [NX|W]) :- NX is -X, neg(V, W).

suma([X],[Y],[R]) :- R is X + Y.
suma([X|V], [Y|W], [S|R]) :- S is X + Y, suma(V, W, R).

dot([X],[Y],[R]) :- R is X * Y.
dot([X|V], [Y|W], R) :- M is X*Y, dot(V, W, S), R is S + M.

dist(V, W, R) :- distAux(V, W, S), R is sqrt(S).
distAux([], [], 0).
distAux([X|V], [Y|W], R) :- M is X-Y, C is M ^ 2, distAux(V, W, S), R is C + S.

% Ejercicio 6

columna([[X|F]], [X], [F]).
columna([[X|F]|M], [X|C], [F|R]) :- columna(M, C, R).

transpuesta([[A]|B], [T]) :- columna([[A]|B], T, _).
transpuesta(M, [C|T]) :- columna(M, C, R), transpuesta(R,T).

simetrica(M) :- transpuesta(M, M).

suma_mat([[X|F1]], [[Y|F2]], [R]) :- suma([X|F1], [Y|F2], R).
suma_mat([F1|M1], [F2|M2], [S|R]) :- suma(F1, F2, S), suma_mat(M1, M2, R).

producto_fila(_, [[]|_], []).
producto_fila(F, M, [SD|R]) :- columna(M, C, RN), dot(F, C, SD), producto_fila(F, RN, R).

producto([], _, []).
producto([F1|M1], N, [R|P]) :- producto_fila(F1, N, R), producto(M1, N, P).
