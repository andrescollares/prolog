%EJ 1 ---------------------------------------------------------
% elegir(?X, ?L1, ?L2) ← La lista L2 resulta de eliminar un elemento de la lista L1.
elegir(X,[X|R1],R1). 
elegir(X,[Y|R1],[Y|R2]) :- elegir(X,R1,R2). %

%--------------------------------------------
% elegirN(?L,+L1,+N,?L2) ← La lista L2 resulta de eliminar N elementos de la lista L1.
% La lista L tiene los elementos eliminados, en el orden en que se seleccionaron. El predicado
% falla si la lista es vacía.
elegirN([], L1, 0, L1).
elegirN([X], L1, 1, L2) :- member(X, L1), elegir(X, L1, L2).
elegirN([X|R], L1, N, L2) :- member(X, L1), elegir(X, L1, L1X), N1 is N - 1, elegirN(R, L1X, N1, L2).

%--------------------------------------------
% suma(+L,?S) ← S es la suma de los elementos de la lista L.
suma([X], X).
suma([X|L], S) :- suma(L, R), S is X + R.

%--------------------------------------------
% fila_matriz(+N, +E, ?F) <- F es una fila de N celdas, donde cada celda tiene el valor E.
fila_matriz(0, _, []).
fila_matriz(N, E, [X|F]) :- N1 is N - 1, X = E, fila_matriz(N1, E, F).

% matriz(+M,+N,+E,?A) ← A es una matriz de M filas y N columnas, donde cada celda tiene el valor E.
% La matriz se representa mediante una lista de M filas, donde cada fila es una lista de N celdas.
matriz(0, _, _, []).
matriz(M, N, E, [[X|F]|A]) :- fila_matriz(N, E, [X|F]), M1 is M - 1, matriz(M1, N, E, A).

%--------------------------------------------
% valor_celda(+I,+J,+A,?E) ← E es el contenido de la celda (I,J) de la matriz A.
valor_celda(1, 1, [[E|_]|_], E).
valor_celda(1, J, [[_|F]|M], E) :- J > 1, J1 is J - 1, valor_celda(1, J1, [F|M], E).
valor_celda(I, J, [[_|_]|M], E) :- I > 1, I1 is I - 1, valor_celda(I1, J, M, E).

%--------------------------------------------
% fila(+N,+M,?F) ← F es la fila N-ésima de la matriz
fila(N, [[_|_]|M], F) :- N > 1, N1 is N - 1, fila(N1, M, F).
fila(1, [[X|R]|_], [X|R]).

%--------------------------------------------
% columna(+M, ?C, ?R) <- R es el resultado de quitar la primera columna C de la matriz M
columna([X|F], [X], F).
columna([[M|E]|_], [M|C], [E|F]):- columna(E, C, F).

% col(+N,+M,?C) ← C es la columna N-ésima de la matriz.
col(N, M, C) :- N1 is N - 1, columna(M, _, R), col(N1, R, C).
col(1, M, C) :- columna(M, C, _).

%--------------------------------------------
% transpuesta(+M, ?T) ← T es la matriz transpuesta de M
transpuesta([[A|B]],[T]):- columna([[A]|B], T, _).
transpuesta(M, [C|T]):- columna(M, C, R), transpuesta(R, T).

%--------------------------------------------
% numeros(+Inicio, +Fin, ?Lista) ← Lista es una lista ordenada de los números entre Inicio y Fin.
% Si Inicio es mayor que fin, el predicado falla.
numeros(X,Y,[X,Y]):- X is Y-1.
numeros(X,Y,[X |L1]):- X < Y, X1 is X+1, numeros(X1, Y, L1).

%EJ 2 ---------------------------------------------------------
% primos(+N,?Primos) ← Primos es una lista de los números primos menores que N,
% encontrados utilizando la Criba de Eratóstenes.
primos(N, Primos) :- numeros(2, N, L), eliminar_multiplos(L, Primos, N).

% eliminar_multiplos_x(+X, +L, ?S) <- S es el resultado de eliminar todos los múltiplos de X
% de la lista L.
eliminar_multiplos_X(_, [], []).
eliminar_multiplos_X(X, [A|L], S) :- A mod X =:= 0, eliminar_multiplos_X(X, L, S).
eliminar_multiplos_X(X, [A|L], [A|S]) :- A mod X =\= 0, eliminar_multiplos_X(X, L, S).

% eliminar_multiplos(+L, ?R, +N) <- R es el resultado de eliminar todos los numeros compuestos
% de la lista L, N es el último número en la lista L.
eliminar_multiplos([X|L], [X|L], N) :- X*X > N.
eliminar_multiplos([X|L], [X|R], N) :- X*X =< N, eliminar_multiplos_X(X, L, S), eliminar_multiplos(S, R, N).


%EJ 3 ---------------------------------------------------------
% caminito(+N,+Muros,+Inicial,+Final,?Caminito) ← Caminito es una
% secuencia de posiciones de la forma pos(Fila,Columna), correspondiente a un camino
% entre la casilla Inicial y la casilla Final, ambas denotadas también como
% pos(Fila,Columna). La lista de Muros está especificada también como una lista de
% posiciones.
caminito(_, _, pos(X,Y), pos(X,Y), [pos(X,Y)]).
caminito(N, Muros, pos(Ix,Iy), pos(Fx,Fy), [pos(Ix,Iy)|R]) :- vecinos_x(pos(Ix,Iy), Muros, N, pos(Sigx,Iy)), caminito(N, [pos(Sigx,Iy)|Muros], pos(Sigx,Iy), pos(Fx,Fy), R).
caminito(N, Muros, pos(Ix,Iy), pos(Fx,Fy), [pos(Ix,Iy)|R]) :- vecinos_y(pos(Ix,Iy), Muros, N, pos(Ix,Sigy)), caminito(N, [pos(Ix,Sigy)|Muros], pos(Ix,Sigy), pos(Fx,Fy), R).

pos(_, _).

% vecinos_x(+P, +Muros, +N, ?PSiguiente) <- PSiguiente corresponde a una casilla valida que se
% encuentra a distancia uno en sentido horizontal a la casilla P.  
% N es el borde del tablero y Muros es una lista de casillas no validas.
vecinos_x(pos(Ix,Iy), Muros, N, pos(Sigx,Iy)):- Ix < N, Sigx is Ix + 1, not(member(pos(Sigx,Iy), Muros)).
vecinos_x(pos(Ix,Iy), Muros, _, pos(Sigx,Iy)):- Ix > 1, Sigx is Ix - 1, not(member(pos(Sigx,Iy), Muros)).

% vecinos_y(+P, +Muros, +N, ?PSiguiente) <- PSiguiente corresponde a una casilla valida que se
% encuentra a distancia uno en sentido vertical a la casilla P.  
% N es el borde del tablero y Muros es una lista de casillas no validas.
vecinos_y(pos(Ix,Iy), Muros, N, pos(Ix,Sigy)):- Iy < N, Sigy is Iy + 1, not(member(pos(Ix,Sigy), Muros)).
vecinos_y(pos(Ix,Iy), Muros, _, pos(Ix,Sigy)):- Iy > 1, Sigy is Iy - 1, not(member(pos(Ix,Sigy), Muros)).
