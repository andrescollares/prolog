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

indice_jugador(x, 0).
indice_jugador(o, 1).


% Hace el movimiento a partir del Estado inicial del tablero, las coordenadas origen y destino y el tipo de movimiento
% el tipo de movimiento puede ser normal o con_captura. En el segundo caso, el movimiento debe incluir una captura, o fallar.
% Tablero y Tablero2 tiene el mismo término, solamente se utiliza para poder consultar la variable de salida desde el bridge

% hacer_movimiento(+Tablero, +FilaOrigen,+ColumnaOrigen,+FilaDestino,+ColumnaDestino,+TipoMovimiento,-Estado2).

%o Yi e Yf son iguales o Xi y Xf son iguales.

% captura hacia la izquierda
capturar_fichas(X, Y, Tablero, Jugador, Oponente, Capturas) :-
    X > 2,
    \+ (X == 5,Y == 3),
    arg(Y, Tablero, Fila),

    X1 is X - 1,
    arg(X1, Fila, Oponente),
    
    X2 is X - 2,
    arg(X2, Fila, Jugador),

    nb_setarg(X1, Fila, -),
    nb_setarg(1, Capturas, 0),
    fail.

% captura hacia la derecha
capturar_fichas(X, Y, Tablero, Jugador, Oponente, Capturas) :-
    X < 4,
    \+ (X == 1,Y == 3),

    arg(Y, Tablero, Fila),

    X1 is X + 1,
    arg(X1, Fila, Oponente),
    
    X2 is X + 2,
    arg(X2, Fila, Jugador),

    nb_setarg(X1, Fila, -),
    nb_setarg(1, Capturas, 0),
    fail.

% captura hacia arriba
capturar_fichas(X, Y, Tablero, Jugador, Oponente, Capturas) :-
    Y > 2,
    \+ (X == 3,Y == 5),

    Y1 is Y - 1,
    arg(Y1, Tablero, Fila1),
    arg(X, Fila1, Oponente),
    
    Y2 is Y - 2,
    arg(Y2, Tablero, Fila2),
    arg(X, Fila2, Jugador),

    nb_setarg(X, Y1, -),
    nb_setarg(1, Capturas, 0),
    fail.

% captura hacia abajo
capturar_fichas(X, Y, Tablero, Jugador, Oponente, Capturas) :-
    Y < 4,
    \+ (X == 3,Y == 1),

    Y1 is Y + 1,
    arg(Y1, Tablero, Fila1),
    arg(X, Fila1, Oponente),
    
    Y2 is Y + 2,
    arg(Y2, Tablero, Fila2),
    arg(X, Fila2, Jugador),

    nb_setarg(X, Fila1, -),
    nb_setarg(1, Capturas, 0),
    fail.

capturar_fichas(_, _, _, _, _, _).
 


capturar_abajo(X, Y, Tablero, Jugador, Oponente, Capturas) :-
    Y < 4,
    \+ (X == 3,Y == 1),

    Y1 is Y + 1,
    arg(Y1, Tablero, Fila1),
    arg(X, Fila1, Oponente),

    Y2 is Y + 2,
    arg(Y2, Tablero, Fila2),
    arg(X, Fila2, Jugador),

    setarg(X, Fila1, -),
    setarg(1, Capturas, 0),
    
    capturar_izquierda(X, Y, Tablero, Jugador, Oponente, Capturas).

capturar_abajo(X, Y, Tablero, Jugador, Oponente, Capturas) :-
%    not abajo falta???
    capturar_izquierda(X, Y, Tablero, Jugador, Oponente, Capturas).


capturar_izquierda(X, Y, Tablero, Jugador, Oponente, Capturas) :-
    X > 2,
    \+ (X == 5,Y == 3),
    arg(Y, Tablero, Fila),

    X1 is X - 1,
    arg(X1, Fila, Oponente),

    X2 is X - 2,
    arg(X2, Fila, Jugador),

    setarg(X1, Fila, -),
    setarg(1, Capturas, 0),

    capturar_arriba(X, Y, Tablero, Jugador, Oponente, Capturas).

capturar_izquierda(X, Y, Tablero, Jugador, Oponente, Capturas) :-
    capturar_arriba(X, Y, Tablero, Jugador, Oponente, Capturas).






capturar_arriba(X, Y, Tablero, Jugador, Oponente, Capturas) :-
    Y > 2,
    \+ (X == 3,Y == 5),

    Y1 is Y - 1,
    arg(Y1, Tablero, Fila1),
    arg(X, Fila1, Oponente),
    
    Y2 is Y - 2,
    arg(Y2, Tablero, Fila2),
    arg(X, Fila2, Jugador),

    setarg(X, Y1, -),
    setarg(1, Capturas, 0),

    capturar_derecha(X, Y, Tablero, Jugador, Oponente, Capturas).


capturar_arriba(X, Y, Tablero, Jugador, Oponente, Capturas) :-
    capturar_derecha(X, Y, Tablero, Jugador, Oponente, Capturas).


capturar_derecha(X, Y, Tablero, Jugador, Oponente, Capturas) :-
    X < 4,
    \+ (X == 1,Y == 3),

    arg(Y, Tablero, Fila),

    X1 is X + 1,
    arg(X1, Fila, Oponente),
    
    X2 is X + 2,
    arg(X2, Fila, Jugador),

    setarg(X1, Fila, -),
    setarg(1, Capturas, 0).


capturar_derecha(_, _, _, _, _, _).



 
% hacer movimiento horizontal
hacer_movimiento(Estado, Y, Xi, Y, Xf, Movimiento, Estado2) :-
    \+ (Xf < Xi - 1), 
    \+ (Xf > Xi + 1),
    
    arg(1, Estado, Tablero),
    arg(Y, Tablero, FilaI),
    arg(Xi, FilaI, Jugador),
    
    arg(Y, Tablero, FilaF),
    arg(Xf, FilaF, -),

    
    oponente(Jugador, Oponente),

    Capturas = capturas(1),
    capturar_izquierda(Xf, Y, Tablero, Jugador, Oponente, Capturas),

    arg(1, Capturas, ResCapturas),
    
    \+ (ResCapturas = 1, Movimiento = con_captura),

    setarg(Xi, FilaI, -),
    setarg(Xf, FilaF, Jugador),

    indice_jugador(Jugador, IndiceJugador),
    IndiceCapturas is 4 + IndiceJugador,
    arg(IndiceCapturas, Estado, Cap),
    CapActualizado is Cap + ResCapturas,

    setarg(IndiceCapturas, Estado, CapActualizado),
    
    Estado2 = Estado.
    
% hacer movimiento vertical
hacer_movimiento(Estado, Yi, X, Yf, X, Movimiento, Estado2) :-
    \+ (Yf < Yi - 1),
    \+ (Yf > Yi + 1),
    
    arg(1, Estado, Tablero),
    arg(Yi, Tablero, FilaI),
    arg(X, FilaI, Jugador),
    
    arg(Yf, Tablero, FilaF),
    arg(X, FilaF, -),
    
    oponente(Jugador, Oponente),

    Capturas = capturas(1),
   
    capturar_izquierda(X, Yf, Tablero, Jugador, Oponente, Capturas),
    arg(1, Capturas, ResCapturas),

    \+ (ResCapturas = 1, Movimiento = con_captura),

    setarg(X, FilaI, -),
    setarg(X, FilaF, Jugador),

    indice_jugador(Jugador, IndiceJugador),
    IndiceCapturas is 4 + IndiceJugador,
    arg(IndiceCapturas, Estado, Cap),
    CapActualizado is Cap + ResCapturas,

    setarg(IndiceCapturas, Estado, CapActualizado),

    Estado2 = Estado.

% captura hacia la izquierda
hay_captura_ficha(X, Y, Tablero, Jugador, Oponente) :-
    X > 2,
    \+ (X == 5,Y == 3),
    arg(Y, Tablero, Fila),

    X1 is X - 1,
    arg(X1, Fila, Oponente),
    
    X2 is X - 2,
    arg(X2, Fila, Jugador).

% captura hacia la derecha
hay_captura_ficha(X, Y, Tablero, Jugador, Oponente) :-
    X < 4,
    \+ (X == 1,Y == 3),

    arg(Y, Tablero, Fila),

    X1 is X + 1,
    arg(X1, Fila, Oponente),
    
    X2 is X + 2,
    arg(X2, Fila, Jugador).

% captura hacia arriba
hay_captura_ficha(X, Y, Tablero, Jugador, Oponente) :-
    Y > 2,
    \+ (X == 3,Y == 5),

    Y1 is Y - 1,
    arg(Y1, Tablero, Fila1),
    arg(X, Fila1, Oponente),
    
    Y2 is Y - 2,
    arg(Y2, Tablero, Fila2),
    arg(X, Fila2, Jugador).

% captura hacia abajo
hay_captura_ficha(X, Y, Tablero, Jugador, Oponente) :-
    Y < 4,
    \+ (X == 3,Y == 1),

    Y1 is Y + 1,
    arg(Y1, Tablero, Fila1),
    arg(X, Fila1, Oponente),
    
    Y2 is Y + 2,
    arg(Y2, Tablero, Fila2),
    arg(X, Fila2, Jugador).


hay_mov_cap_ficha(X, Y, Tablero, Jugador, Oponente) :-
    X > 1,
    X1 is X - 1,

    arg(Y, Tablero, Fila),
    arg(X1, Fila, -),

    hay_captura_ficha(X1, Y, Tablero, Jugador, Oponente).

hay_mov_cap_ficha(X, Y, Tablero, Jugador, Oponente) :-
    X < 5,
    X1 is X + 1,
    arg(Y, Tablero, Fila),
    arg(X1, Fila, -),

    hay_captura_ficha(X1, Y, Tablero, Jugador, Oponente).

hay_mov_cap_ficha(X, Y, Tablero, Jugador, Oponente) :-
    Y > 1,
    Y1 is Y - 1,
    arg(Y1, Tablero, Fila),
    arg(X, Fila, -),

    hay_captura_ficha(X, Y1, Tablero, Jugador, Oponente).

hay_mov_cap_ficha(X, Y, Tablero, Jugador, Oponente) :-
    Y < 5,
    Y1 is Y + 1,
    arg(Y1, Tablero, Fila),
    arg(X, Fila, -),

    hay_captura_ficha(X, Y1, Tablero, Jugador, Oponente).

hay_posible_captura(Estado, Jugador) :- 
    oponente(Jugador, Oponente),
    arg(1, Estado, Tablero),
    % member(N, [1,2,3,4,5]), 
    between(1, 5, Y),
    arg(Y, Tablero, Fila), 

    between(1, 5, X),
    arg(X, Fila, Jugador_Aux),

    Jugador == Jugador_Aux,
    
    hay_mov_cap_ficha(X, Y, Tablero, Jugador, Oponente).

% hay_movimiento: es exitoso si hay algún movimient posible para el jugador
% hay_movimiento(+Estado,+Jugador).

hay_movimiento_ficha(X, Y, Tablero) :-
    X > 1,
    X1 is X - 1,
    arg(Y, Tablero, Fila),
    arg(X1, Fila, -).

hay_movimiento_ficha(X, Y, Tablero) :-
    X < 5,
    X1 is X + 1,
    arg(Y, Tablero, Fila),
    arg(X1, Fila, -).

hay_movimiento_ficha(X, Y, Tablero) :-
    Y > 1,
    Y1 is Y - 1,
    arg(Y1, Tablero, Fila),
    arg(X, Fila, -).

hay_movimiento_ficha(X, Y, Tablero) :-
    Y < 5,
    Y1 is Y + 1,
    arg(Y1, Tablero, Fila),
    arg(X, Fila, -).


hay_movimiento(Estado, Jugador) :- 
    % member(N, [1,2,3,4,5]), 
    arg(1, Estado, Tablero),
    between(1, 5, Y),
    arg(Y, Tablero, Fila), 

    between(1, 5, X),
    arg(X, Fila, Jugador_Aux),

    Jugador == Jugador_Aux,
    hay_movimiento_ficha(X, Y, Tablero).

% Casos de prueba
% m(f(o, x, x, -, x),f(),f(),f(),f())
% m(f(o, x, x, o, x),f(o, -, o, o, o),f(),f(),f())

% 
%estado(m(f(o, -, x, o, -), f(o, x, o, o, o), f(o, o, o, o, o), f(), f())), 1, 1, 1, 2, normal, estado2)

% mejor_movimiento: dado un estado, un jugador, un nivel para minimax, y una estrategia, devuelve la mejor jugada posible
% Estrategia es solamente un átomo que se le asigna para poder implementar más de una estrategia
% mejor_movimiento(+Estado,+Jugador,+NivelMinimax,+Estrategia,-Estado2):-
