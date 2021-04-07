% Ejercicio 1

%region(Nombre, Color, ColoresVecinos).

%mapa([region(_,_,_)]).

%Mapa se encuentra coloreado con Colores de forma que no hay dos vecinos con el mismo color.
colorear(Mapa, Colores) :- genera(Mapa, Colores), sin_conflicto(Mapa).

genera([], _).
% genera(Mapa, Colores) :- 


%que los colores de los vecinos no sean el mismo que el de la region en la que estoy parado
sin_conflicto([region(N, C, [CV|RCV])|RR]) :- C \= CV, sin_conflicto([region(N, C, RCV)|RR]).

sin_conflicto([region(_, _, [])|RR]) :- sin_conflicto(RR).

sin_conflicto([]).

%????? Como recorro los vecinos??, la unica relación entre regiones que tengo en mapa son los colores que tienen sus vecinos.

% Ejercicio 2

kreinas(K, N, Qs) :- K =< N, primer_tablero(K, N, LKs), permutation(LKs, Qs), segura(Qs).

pool_numeros(0, []).
pool_numeros(N, [N|Pool]) :- N >= 1, N1 is N - 1, pool_numeros(N1, Pool).

select_K_numeros(0, _, []).
select_K_numeros(K, Pool, [X|R]) :- K >= 1, select(X, Pool, Pool_sin_X), K1 is K - 1, select_K_numeros(K1, Pool_sin_X, R).

lista_de_ceros(0, []).
lista_de_ceros(N, [0|R]) :- N >= 1, N1 is N - 1, lista_de_ceros(N1, R).

primer_tablero(K, N, Primer_tablero) :- pool_numeros(N, PoolPosible), select_K_numeros(K, PoolPosible, Pool), NmK is N - K, lista_de_ceros(NmK, LCeros), append(Pool, LCeros, Primer_tablero). 

segura([Q|Qs]) :- no_ataca(Q, Qs), segura(Qs).
segura([]).

no_ataca(0, _).
no_ataca(Q, Qs) :- Q =\= 0, no_ataca(Q, 1, Qs).
no_ataca(_, _, []).
no_ataca(X, N, [Y|Ys]) :- Y =\= 0, X =\= Y+N, X =\= Y-N, N1 is N+1, no_ataca(X, N1, Ys).
no_ataca(X, N, [0|Ys]) :- N1 is N+1, no_ataca(X, N1, Ys). 

% Ejercicio 3

peones(Movs) :- tablero_inicial(Tab), jugar(Tab, Movs).

tablero_inicial([b,b,b,0,n,n,n]).

% Los unicos movimientos posibles son si n tiene un cero a la izquierda, o si b tiene un cero a la derecha,
% o si hay 0,b,n, o b,n,0
% Nunca puedo hacer un movimiento que me deje 2 blancas o 2 negras juntas.
% Ademas siempre que hago un movimiento de un tipo, sigue un movimiento del otro, además cuando muevo una ficha de un color, le sigue una ficha de otro color.

jugar(Tablero, [Mov|Movs]) :- jugada(Tablero, Mov, Tablero_res), jugar(Tablero_res, Movs).
jugar(Tablero, _) :- tablero_ganador(Tablero).

jugada(Tablero, Mov, Resultado) :- movimiento(Tablero, Resultado, Mov).
jugada([X|Tablero], Mov, [X|Tablero_res]) :- jugada(Tablero, Mov, Tablero_res).

movimiento([b,0|R], [0,b|R], bDer).
movimiento([b,n,0|R], [0,n,b|R], bSal).

movimiento([0,n|R], [n,0|R], nIzq).
movimiento([0,b,n|R], [n,b,0|R], nSal).

tablero_ganador([n,n,n,0,b,b,b]).

% Ejercicio 4

% Un hombre solo puede pasar con su pareja, viajar solo, o con otro hombre.
% Las orillas las tomo como dos conjuntos, y los viajes son una función.

parejas(Viajes) :- orilla_der_inicio(Orilla_der), orilla_izq_inicio(Orilla_izq), movimiento_der_izq(Orilla_der, Orilla_izq, Viajes).

orilla_der_inicio([h1,m1,h2,m2,h3,m3]).
orilla_izq_inicio([]).

hombre(h1).
hombre(h2).
hombre(h3).

mujer(m1).
mujer(m2).
mujer(m3).

pareja(h1, m1).
pareja(h2, m2).
pareja(h3, m3).
pareja(m1, h1).
pareja(m2, h2).
pareja(m3, h3).

movimiento_der_izq(Orilla_der, Orilla_izq, [Viaje| Viajes]) :- cruce_der_izq(Orilla_der, Pasajeros, Viaje, Res_orilla_der), append(Pasajeros, Orilla_izq, Res_orilla_izq), movimiento_izq_der(Res_orilla_der, Res_orilla_izq, Viajes).

movimiento_izq_der(Orilla_der, Orilla_izq, [Viaje| Viajes]) :- cruce_izq_der(Orilla_izq, Pasajeros, Viaje, Res_orilla_izq), append(Pasajeros, Orilla_der, Res_orilla_der), movimiento_der_izq(Res_orilla_der, Res_orilla_izq, Viajes).
movimiento_izq_der(_, Orilla_izq, []) :- problema_resuelto(Orilla_izq).

% Debería seleccionar una o dos personas para cada viaje.

% cruce_der_izq(Orilla_der, Pasajero, Viaje, Result_orilla_der) :- cruce_der_izq1(Orilla_der, Viaje, Pasajero, Result_orilla_der).
cruce_der_izq(Orilla_der, Pasajeros, Viaje, Result_orilla_der) :- cruce_der_izq2(Orilla_der, Viaje, Pasajeros, Result_orilla_der).

cruce_der_izq2(Orilla_der, ViajeR, [X, Y], Result_orilla_der) :- select(X, Orilla_der, Rest_orilla_der), select(Y, Rest_orilla_der, Result_orilla_der), viaje(X, Y, Viaje), string_concat(di, Viaje, ViajeR). 
cruce_der_izq1(Orilla_der, ViajeR, [X], Result_orilla_der) :- length(Orilla_der, Cantidad), Cantidad < 6, select(X, Orilla_der, Result_orilla_der), viaje(X, Viaje), string_concat(di, Viaje, ViajeR).

cruce_izq_der(Orilla_izq, Pasajero, Viaje, Result_orilla_izq) :- cruce_izq_der1(Orilla_izq, Viaje, Pasajero, Result_orilla_izq).
% cruce_izq_der(Orilla_izq, Pasajeros, Viaje, Result_orilla_izq) :- cruce_izq_der2(Orilla_izq, Viaje, Pasajeros, Result_orilla_izq).

cruce_izq_der2(Orilla_izq, ViajeR, [X, Y], Result_orilla_izq) :- length(Orilla_izq, Cantidad), Cantidad > 2, Cantidad < 6, select(X, Orilla_izq, Rest_orilla_izq), select(Y, Rest_orilla_izq, Result_orilla_izq), viaje(X, Y, Viaje), string_concat(id, Viaje, ViajeR). 
cruce_izq_der1(Orilla_izq, ViajeR, [X], Result_orilla_izq) :- length(Orilla_izq, Cantidad), Cantidad < 6, select(X, Orilla_izq, Result_orilla_izq), viaje(X, Viaje), string_concat(id, Viaje, ViajeR). 

viaje(X, Y, hh) :- hombre(X), hombre(Y).
viaje(X, Y, mm) :- mujer(X), mujer(Y).
viaje(X, Y, pareja) :- pareja(X, Y).
viaje(X, soloh) :- hombre(X).
viaje(X, solom) :- mujer(X).

problema_resuelto(Orilla_izq) :- member(h1, Orilla_izq), member(h2, Orilla_izq), member(h3, Orilla_izq), member(m1, Orilla_izq), member(m2, Orilla_izq), member(m3, Orilla_izq). 

% Ejercicio 5
% Genero soluciones usando filas y luego chequeo que se cumplan las condiciones de las columnas

% Esp es una especificación del problema, seria dos conjuntos de condiciones (de filas y de columnas respectivamente), 
puzle([Filas|Columnas], Matriz) :- length(Columnas, NroColumnas), generar(Filas, Matriz, NroColumnas), verificar(Matriz, Columnas).

generar([], [], _).
generar([Condicion|R], [Fila|Matriz], NroColumnas) :- generar_fila(Condicion, Fila, NroColumnas), generar(R, Matriz, NroColumnas).

generar_fila([LargoSec|R], [Secuencia|SubR], NroColumnas) :- NroColumnas >= LargoSec, generar_secuencia(LargoSec, Secuencia), NroColumnasRestantes is NroColumnas - LargoSec, generar_fila(R, SubR, NroColumnasRestantes).
generar_fila([LargoSec|R], [b|Resultado], NroColumnas) :- NroColumnas > 0, NroColumnas1 is NroColumnas - 1, generar_fila([LargoSec|R], Resultado, NroColumnas1).
generar_fila([], [], 0).

generar_secuencia(0, []).
generar_secuencia(N, [n|Res]) :- N > 0, N1 is N - 1, generar_secuencia(N1, Res).

columna([[X|F]], [X], [F]).
columna([[X|F]|M], [X|C], [F|R]) :- columna(M, C, R).

verificar(Matriz, [Condicion|R]) :- columna(Matriz, Col, Resto), verificar_columna(Col, Condicion), verificar(Resto, R).

verificar_columna([n|RColumna], [LargoSec|Cond]) :- verificar_secuencia([n|RColumna], LargoSec, Resto), verificar_columna(Resto, Cond).
verificar_columna([b|RColumna], Condiciones) :- verificar_columna(RColumna, Condiciones).
verificar_columna([], []).


verificar_secuencia([n|RColumna], LargoSec, Resto) :- LargoSec1 is LargoSec - 1, verificar_secuencia([RColumna], LargoSec1, Resto).
verificar_secuencia(Resto, 0, Resto).
