% El tablero es un damero de 5 x 5. Cada celda tiene el valor negra, blanca, o vacía.
% Vamos a utilizar, por razones de eficiencia, matrices representadas como functores en Prolog (Pr�ctico 8)
% Un tablero tiene la forma 

f(_,_,_,_,_).
c(_,_,_,_,_).
m(f(_,_,_,_,_),f(_,_,_,_,_),f(_,_,_,_,_),f(_,_,_,_,_),f(_,_,_,_,_),_,_,_,_,_).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% PREDICADOS QUE SE INVOCAN DESDE EL BRIDGE, NECESARIAMENTE DEBEN IMPLEMENTARSE ************************************************

% Hace el movimiento a partir del Estado inicial del tablero, las coordenadas origen y destino y el tipo de movimiento
% el tipo de movimiento puede ser normal o con_captura. En el segundo caso, el movimiento debe incluir una captura, o fallar.
% Tablero y Tablero2 tiene el mismo término, solamente se utiliza para poder consultar la variable de salida desde el bridge
% hacer_movimiento(+Tablero, +FilaOrigen,+ColumnaOrigen,+FilaDestino,+ColumnaDestino,+TipoMovimiento,-Estado2).



% hay_posible_captura(+Estado, +Jugador): dado un Estado y un jugador, veo si alguno de los movimientos que puede realizar lleva a una captura

%Hay captura cuando hay un _,O,-,X,O o O,-,-,X,O y vos sos O.
%Tambien cuando hay una "L" 
% x
% o
% - x

hay_posible_captura_fila(f(_,x,-,o,x), x).
hay_posible_captura_fila(f(x,-,o,x,_), x).
hay_posible_captura_fila(f(x,o,-,x,_), x).
hay_posible_captura_fila(f(_,x,o,-,x), x).

hay_posible_captura_fila(f(_,o,-,x,o), o).
hay_posible_captura_fila(f(o,-,x,o,_), o).
hay_posible_captura_fila(f(o,x,-,o,_), o).
hay_posible_captura_fila(f(_,o,x,-,o), o).

% hay_movimiento: es exitoso si hay algún movimient posible para el jugador
% hay_movimiento(+Estado,+Jugador).


hay_movimiento(m(Fila1, Fila2, Fila3, Fila4, Fila5,1,2,3,4,5), Jugador) :- hay_movimiento_fila(Fila1, Jugador).
hay_movimiento(m(Fila1, Fila2, Fila3, Fila4, Fila5,1,2,3,4,5), Jugador) :- hay_movimiento_fila(Fila2, Jugador).
hay_movimiento(m(Fila1, Fila2, Fila3, Fila4, Fila5,1,2,3,4,5), Jugador) :- hay_movimiento_fila(Fila3, Jugador).
hay_movimiento(m(Fila1, Fila2, Fila3, Fila4, Fila5,1,2,3,4,5), Jugador) :- hay_movimiento_fila(Fila4, Jugador).
hay_movimiento(m(Fila1, Fila2, Fila3, Fila4, Fila5,1,2,3,4,5), Jugador) :- hay_movimiento_fila(Fila5, Jugador).

hay_movimiento_fila(f(A,B,C,D,E), Jugador) :- A is Jugador, B is '-'.

hay_movimiento_columna(c(A,B,C,D,E), Jugador) :-

/*

5 parametros finales del estado:
1 y 2- el número de turnos seguidos sin movimiento de ambos jugadores (x primero?)
3 y 4- el número de jugadas sin capturar de ambos jugadores
5- la fase en la que se está jugando (1 indica que se están insertando fichas, y 2 que se están moviendo)

*/

% mejor_movimiento: dado un estado, un jugador, un nivel para minimax, y una estrategia, devuelve la mejor jugada posible
% Estrategia es solamente un átomo que se le asigna para poder implementar más de una estrategia
% mejor_movimiento(+Estado,+Jugador,+NivelMinimax,+Estrategia,-Estado2):-




%Ejemplo de estado
%estado(m(f(-,-,-,-,-),f(-,-,o,x,-),f(-,-,o,x,-),f(x,o,o,x,-),f(x,x,-,-,o)),0,0,3,5,2).


