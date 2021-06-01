% El tablero es un damero de 5 x 5. Cada celda tiene el valor negra, blanca, o vacía.
% Vamos a utilizar, por razones de eficiencia, matrices representadas como functores en Prolog (Pr�ctico 8)
% Un tablero tiene la forma 

f(_,_,_,_,_).

m(f(_,_,_,_,_),f(_,_,_,_,_),f(_,_,_,_,_),f(_,_,_,_,_),f(_,_,_,_,_),_,_,_,_,_).

%Ejemplo de estado
%estado(m(f(-,-,-,-,-),f(-,-,o,x,-),f(-,-,o,x,-),f(x,o,o,x,-),f(x,x,-,-,o)),0,0,3,5,2).

/*
5 parametros finales del estado:
1 y 2- el número de turnos seguidos sin movimiento de ambos jugadores (x primero?)
3 y 4- el número de jugadas sin capturar de ambos jugadores
5- la fase en la que se está jugando (1 indica que se están insertando fichas, y 2 que se están moviendo)
*/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% PREDICADOS QUE SE INVOCAN DESDE EL BRIDGE, NECESARIAMENTE DEBEN IMPLEMENTARSE ************************************************

oponente(o, x).
oponente(x, o).

% Hace el movimiento a partir del Estado inicial del tablero, las coordenadas origen y destino y el tipo de movimiento
% el tipo de movimiento puede ser normal o con_captura. En el segundo caso, el movimiento debe incluir una captura, o fallar.
% Tablero y Tablero2 tiene el mismo término, solamente se utiliza para poder consultar la variable de salida desde el bridge
% hacer_movimiento(+Tablero, +FilaOrigen,+ColumnaOrigen,+FilaDestino,+ColumnaDestino,+TipoMovimiento,-Estado2).


% captura hacia la izquierda
hay_captura_ficha(X, Y, Estado, Jugador, Oponente) :-
    X > 2,
    \+ (X == 5,Y == 3),
    arg(Y, Estado, Fila),

    X1 is X - 1,
    arg(X1, Fila, Oponente),
    
    X2 is X - 2,
    arg(X2, Fila, Jugador).

% captura hacia la derecha
hay_captura_ficha(X, Y, Estado, Jugador, Oponente) :-
    X < 4,
    \+ (X == 1,Y == 3),

    arg(Y, Estado, Fila),

    X1 is X + 1,
    arg(X1, Fila, Oponente),
    
    X2 is X + 2,
    arg(X2, Fila, Jugador).

% captura hacia arriba
hay_captura_ficha(X, Y, Estado, Jugador, Oponente) :-
    Y > 2,
    \+ (X == 3,Y == 5),

    Y1 is Y - 1,
    arg(Y1, Estado, Fila1),
    arg(X, Fila1, Oponente),
    
    Y2 is Y - 2,
    arg(Y2, Estado, Fila2),
    arg(X, Fila2, Jugador).

% captura hacia abajo
hay_captura_ficha(X, Y, Estado, Jugador, Oponente) :-
    Y < 4,
    \+ (X == 3,Y == 1),

    Y1 is Y + 1,
    arg(Y1, Estado, Fila1),
    arg(X, Fila1, Oponente),
    
    Y2 is Y + 2,
    arg(Y2, Estado, Fila2),
    arg(X, Fila2, Jugador).


hay_mov_cap_ficha(X, Y, Estado, Jugador, Oponente) :-
    X > 1,
    X1 is X - 1,

    arg(Y, Estado, Fila),
    arg(X1, Fila, -),

    hay_captura_ficha(X1, Y, Estado, Jugador, Oponente).

hay_mov_cap_ficha(X, Y, Estado, Jugador, Oponente) :-
    X < 5,
    X1 is X + 1,
    arg(Y, Estado, Fila),
    arg(X1, Fila, -),

    hay_captura_ficha(X1, Y, Estado, Jugador, Oponente).

hay_mov_cap_ficha(X, Y, Estado, Jugador, Oponente) :-
    Y > 1,
    Y1 is Y - 1,
    arg(Y1, Estado, Fila),
    arg(X, Fila, -),

    hay_captura_ficha(X, Y1, Estado, Jugador, Oponente).

hay_mov_cap_ficha(X, Y, Estado, Jugador, Oponente) :-
    Y < 5,
    Y1 is Y + 1,
    arg(Y1, Estado, Fila),
    arg(X, Fila, -),

    hay_captura_ficha(X, Y1, Estado, Jugador, Oponente).

hay_posible_captura(Estado, Jugador) :- 
    oponente(Jugador, Oponente),
    % member(N, [1,2,3,4,5]), 
    between(1, 5, Y),
    arg(Y, Estado, Fila), 

    between(1, 5, X),
    arg(X, Fila, Jugador_Aux),

    Jugador == Jugador_Aux,
    hay_mov_cap_ficha(X, Y, Estado, Jugador, Oponente).

% hay_movimiento: es exitoso si hay algún movimient posible para el jugador
% hay_movimiento(+Estado,+Jugador).

hay_movimiento_ficha(X, Y, Estado) :-
    X > 1,
    X1 is X - 1,
    arg(Y, Estado, Fila),
    arg(X1, Fila, -).

hay_movimiento_ficha(X, Y, Estado) :-
    X < 5,
    X1 is X + 1,
    arg(Y, Estado, Fila),
    arg(X1, Fila, -).

hay_movimiento_ficha(X, Y, Estado) :-
    Y > 1,
    Y1 is Y - 1,
    arg(Y1, Estado, Fila),
    arg(X, Fila, -).

hay_movimiento_ficha(X, Y, Estado) :-
    Y < 5,
    Y1 is Y + 1,
    arg(Y1, Estado, Fila),
    arg(X, Fila, -).


hay_movimiento(Estado, Jugador) :- 
    % member(N, [1,2,3,4,5]), 
    between(1, 5, Y),
    arg(Y, Estado, Fila), 

    between(1, 5, X),
    arg(X, Fila, Jugador_Aux),

    Jugador == Jugador_Aux,
    hay_movimiento_ficha(X, Y, Estado).

% Casos de prueba
% m(f(o, x, x, -, x),f(),f(),f(),f())
% m(f(o, x, x, o, x),f(o, -, o, o, o),f(),f(),f())

% mejor_movimiento: dado un estado, un jugador, un nivel para minimax, y una estrategia, devuelve la mejor jugada posible
% Estrategia es solamente un átomo que se le asigna para poder implementar más de una estrategia
% mejor_movimiento(+Estado,+Jugador,+NivelMinimax,+Estrategia,-Estado2):-
