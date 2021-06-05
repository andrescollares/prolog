% El tablero es un damero de 5 x 5. Cada celda tiene el valor negra, blanca, o vacía.
% Vamos a utilizar, por razones de eficiencia, matrices representadas como functores en Prolog (Pr�ctico 8)
% Un tablero tiene la forma 

f(_,_,_,_,_).

m(f(_,_,_,_,_),f(_,_,_,_,_),f(_,_,_,_,_),f(_,_,_,_,_),f(_,_,_,_,_),_,_,_,_,_).

%Ejemplo de estado
%estado(m(f(-,-,-,-,-),f(-,-,o,x,-),f(-,-,o,x,-),f(x,o,o,x,-),f(x,x,-,-,o)),0,0,3,5,2).

/*
6 parametros del estado:
1- Tablero
2 y 3- el número de turnos seguidos sin movimiento de ambos jugadores (x primero?)
4 y 5- el número de jugadas sin capturar de ambos jugadores
6- la fase en la que se está jugando (1 indica que se están insertando fichas, y 2 que se están moviendo)
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
% hacer movimiento horizontal
hacer_movimiento(Estado, Y, Xi, Y, Xf, _, Estado2) :-
    \+ (Xf < Xi - 1), 
    \+ (Xf > Xi + 1),
    
    arg(1, Estado, Tablero),
    arg(Y, Tablero, FilaI),
    arg(Xi, FilaI, Jugador),
    
    arg(Y, Tablero, FilaF),
    arg(Xf, FilaF, -),

    oponente(Jugador, Oponente),

    Capturas = capturas(1),
    capturar_abajo(Xf, Y, Tablero, Jugador, Oponente, Capturas),
    arg(1, Capturas, 0), !,
    % \+ (ResCapturas = 1, Movimiento = con_captura),

    setarg(Xi, FilaI, -),
    setarg(Xf, FilaF, Jugador),

    indice_jugador(Jugador, IndiceJugador),
    IndiceCapturas is 4 + IndiceJugador,

    setarg(IndiceCapturas, Estado, 0),
    
    Estado2 = Estado.
    
hacer_movimiento(Estado, Y, Xi, Y, Xf, Movimiento, Estado2) :-
    \+ (Xf < Xi - 1), 
    \+ (Xf > Xi + 1),
    \+ (Movimiento = con_captura),
    
    arg(1, Estado, Tablero),
    arg(Y, Tablero, FilaI),
    arg(Xi, FilaI, Jugador),
    
    arg(Y, Tablero, FilaF),
    arg(Xf, FilaF, -),

    setarg(Xi, FilaI, -),
    setarg(Xf, FilaF, Jugador),

    indice_jugador(Jugador, IndiceJugador),
    IndiceCapturas is 4 + IndiceJugador,

    arg(IndiceCapturas, Estado, Cap),
    CapActualizado is Cap + 1,
    setarg(IndiceCapturas, Estado, CapActualizado),
    
    Estado2 = Estado.

% hacer movimiento vertical
hacer_movimiento(Estado, Yi, X, Yf, X, _, Estado2) :-
    \+ (Yf < Yi - 1),
    \+ (Yf > Yi + 1),
    
    arg(1, Estado, Tablero),
    arg(Yi, Tablero, FilaI),
    arg(X, FilaI, Jugador),
    
    arg(Yf, Tablero, FilaF),
    arg(X, FilaF, -),
    
    oponente(Jugador, Oponente),

    Capturas = capturas(1),
    capturar_abajo(X, Yf, Tablero, Jugador, Oponente, Capturas),
    arg(1, Capturas, 0), !,
    % \+ (ResCapturas = 1, Movimiento = con_captura),

    setarg(X, FilaI, -),
    setarg(X, FilaF, Jugador),

    indice_jugador(Jugador, IndiceJugador),
    IndiceCapturas is 4 + IndiceJugador,

    setarg(IndiceCapturas, Estado, 0),

    Estado2 = Estado.

hacer_movimiento(Estado, Yi, X, Yf, X, Movimiento, Estado2) :-
    \+ (Yf < Yi - 1),
    \+ (Yf > Yi + 1),
    \+ (Movimiento = con_captura),
    
    arg(1, Estado, Tablero),
    arg(Yi, Tablero, FilaI),
    arg(X, FilaI, Jugador),
    
    arg(Yf, Tablero, FilaF),
    arg(X, FilaF, -),
    
    setarg(X, FilaI, -),
    setarg(X, FilaF, Jugador),

    indice_jugador(Jugador, IndiceJugador),
    IndiceCapturas is 4 + IndiceJugador,

    arg(IndiceCapturas, Estado, Cap),
    CapActualizado is Cap + 1,

    setarg(IndiceCapturas, Estado, CapActualizado),

    Estado2 = Estado.

% captura hacia abajo
capturar_abajo(X, Y, Tablero, Jugador, Oponente, Capturas) :-
    Y < 4,
    \+ (X == 3,Y == 2),

    Y1 is Y + 1,
    arg(Y1, Tablero, Fila1),
    arg(X, Fila1, Oponente),

    Y2 is Y + 2,
    arg(Y2, Tablero, Fila2),
    arg(X, Fila2, Jugador), !,

    setarg(X, Fila1, -),
    setarg(1, Capturas, 0),
    
    capturar_izquierda(X, Y, Tablero, Jugador, Oponente, Capturas).

capturar_abajo(X, Y, Tablero, Jugador, Oponente, Capturas) :-
    capturar_izquierda(X, Y, Tablero, Jugador, Oponente, Capturas).


capturar_izquierda(X, Y, Tablero, Jugador, Oponente, Capturas) :-
    X > 2,
    \+ (X == 4,Y == 3),
    arg(Y, Tablero, Fila),

    X1 is X - 1,
    arg(X1, Fila, Oponente),

    X2 is X - 2,
    arg(X2, Fila, Jugador), !,

    setarg(X1, Fila, -),
    setarg(1, Capturas, 0),

    capturar_arriba(X, Y, Tablero, Jugador, Oponente, Capturas).

capturar_izquierda(X, Y, Tablero, Jugador, Oponente, Capturas) :-
    capturar_arriba(X, Y, Tablero, Jugador, Oponente, Capturas).


capturar_arriba(X, Y, Tablero, Jugador, Oponente, Capturas) :-
    Y > 2,
    \+ (X == 3,Y == 4),

    Y1 is Y - 1,
    arg(Y1, Tablero, Fila1),
    arg(X, Fila1, Oponente),
    
    Y2 is Y - 2,
    arg(Y2, Tablero, Fila2),
    arg(X, Fila2, Jugador), !,

    setarg(X, Fila1, -),
    setarg(1, Capturas, 0),

    capturar_derecha(X, Y, Tablero, Jugador, Oponente, Capturas).

capturar_arriba(X, Y, Tablero, Jugador, Oponente, Capturas) :-
    capturar_derecha(X, Y, Tablero, Jugador, Oponente, Capturas).

capturar_derecha(X, Y, Tablero, Jugador, Oponente, Capturas) :-
    X < 4,
    \+ (X == 2,Y == 3),

    arg(Y, Tablero, Fila),

    X1 is X + 1,
    arg(X1, Fila, Oponente),
    
    X2 is X + 2,
    arg(X2, Fila, Jugador), !,

    setarg(X1, Fila, -),
    setarg(1, Capturas, 0).




capturar_derecha(_, _, _, _, _, _).


% captura hacia la izquierda
hay_captura_ficha(X, Y, Tablero, Jugador, Oponente) :-
    X > 2,
    \+ (X == 4,Y == 3),
    arg(Y, Tablero, Fila),

    X1 is X - 1,
    arg(X1, Fila, Oponente),
    
    X2 is X - 2,
    arg(X2, Fila, Jugador).

% captura hacia la derecha
hay_captura_ficha(X, Y, Tablero, Jugador, Oponente) :-
    X < 4,
    \+ (X == 2,Y == 3),

    arg(Y, Tablero, Fila),

    X1 is X + 1,
    arg(X1, Fila, Oponente),
    
    X2 is X + 2,
    arg(X2, Fila, Jugador).

% captura hacia arriba
hay_captura_ficha(X, Y, Tablero, Jugador, Oponente) :-
    Y > 2,
    \+ (X == 3,Y == 4),

    Y1 is Y - 1,
    arg(Y1, Tablero, Fila1),
    arg(X, Fila1, Oponente),
    
    Y2 is Y - 2,
    arg(Y2, Tablero, Fila2),
    arg(X, Fila2, Jugador).

% captura hacia abajo
hay_captura_ficha(X, Y, Tablero, Jugador, Oponente) :-
    Y < 4,
    \+ (X == 3,Y == 2),

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

% hay_ficha(Tablero, Jugador) :-
    


gano(Estado, Jugador) :-
    oponente(Jugador, Oponente),
    indice_jugador(Oponente, Indice),
    IndiceTurnos is 2 + Indice,
    arg(IndiceTurnos, Estado, TurnosSinMover),
    TurnosSinMover =:= 3.

gano(Estado, Jugador) :-
    oponente(Jugador, Oponente),
    arg(1, Estado, Tablero),
    hay_ficha(Tablero, Oponente), !,
    fail.
gano(_, _).

hay_ficha(Tablero, Oponente) :-

    between(1, 5, Y),
    arg(Y, Tablero, Fila),

    between(1, 5, X),
    arg(X, Fila, Oponente).
    

es_empate(Estado) :-
    arg(4, Estado, XSinCapturar),
    arg(5, Estado, OSinCapturar),
    XSinCapturar >= 12,
    OSinCapturar >= 12.
    

% mejor_movimiento: dado un estado, un jugador, un nivel para minimax, y una estrategia, devuelve la mejor jugada posible
% Estrategia es solamente un átomo que se le asigna para poder implementar más de una estrategia

% mejor_movimiento(+Estado,+Jugador,+NivelMinimax,+Estrategia,-Estado2).



mejor_movimiento(Estado, Jugador, Nivel, ia_grupo, Estado) :-
    % ?? ValidateInput ??
    arg(6, Estado, Etapa),
    Etapa == 1,
    mejor_movimiento_etapa1(Estado, Jugador, Nivel),
    mejor_movimiento_etapa1(Estado, Jugador, Nivel).



mejor_movimiento(Estado, Jugador, _, _, Estado) :-
    arg(6, Estado, 2),
    gano(Estado, Jugador);

    arg(6, Estado, 2),
    oponente(Jugador, Oponente),
    gano(Estado, Oponente);

    arg(6, Estado, 2),
    es_empate(Estado).

mejor_movimiento(Estado, Jugador, Nivel, ia_grupo,  MejorJugada) :-
    % ?? ValidateInput ??
    arg(6, Estado, 2),
    mejor_movimiento_step(Estado, Jugador, Jugador, Nivel, max,  MejorJugada, _).


mejor_movimiento_step(Estado, JugadorOriginal, Jugador, Nivel, MinMax, MejorJugada, MejorPuntaje) :-
    Nivel > 0, !,
    Nivel1 is Nivel - 1,
    movimientos_posibles(Jugador, Estado, MovimientosPosibles),
    mejor_jugada(Nivel1, JugadorOriginal, Jugador, MinMax, MovimientosPosibles, MejorJugada, MejorPuntaje).

mejor_movimiento_step(_, JugadorOriginal, _, Nivel, _, _, MejorPuntaje) :-
    tablero_final(0, JugadorOriginal, Estado, MejorPuntaje).


% ?- mejor_movimiento(estado(m(f(x, x, -, -, -), f(-, -, -, -, -), f(-, -, -, -, -), f(-, -, -, -, -), f(-, -, -, -, -)), 0,0,0,0,1), x, 3, ia_grupo, BestMove).

movimientos_posibles(Jugador, Estado, MovimientosPosibles) :-
    % findall(PosicionJugador, , PosicionesJugador).
    findall(NuevoEstado, hacer_movimiento_jugador(Estado, Jugador, normal, NuevoEstado), MovimientosPosibles).


hacer_movimiento_jugador(Estado, Jugador, normal, NuevoEstado) :-
    \+ hay_movimiento(Estado, Jugador), !,

    indice_jugador(Jugador, Indice),
    IndiceJug is 2 + Indice,

    arg(IndiceJug, Estado, JugSinMover),
    JugSinMoverAct is JugSinMover + 1,

    arg(IndiceJug, Estado, JugSinMoverAct),
    
    NuevoEstado == Estado.


hacer_movimiento_jugador(Estado, Jugador, TipoMovimiento, NuevoEstado) :-

    arg(1, Estado, Tablero),

    between(1, 5, Y),
    arg(Y, Tablero, Fila),

    between(1, 5, X),
    
    arg(X, Fila, Jugador),

    X1 is X - 1,
    %X1 > 0,
    X2 is X + 1,
    %X2 > 0,
    between(X1, X2, Xf),
    
    Y1 is Y - 1,
    %Y1 > 0,
    Y2 is Y + 1,
    %Y2 < 6,
    between(Y1, Y2, Yf),

    hacer_movimiento(Estado, Y ,X, Yf, Xf, TipoMovimiento, NuevoEstado),
    
    indice_jugador(Jugador, Indice),
    IndiceJug is 4 + Indice,
    arg(IndiceJug, NuevoEstado, 0),

    hacer_movimiento_jugador(Estado, Jugador, con_captura, NuevoEstado).

hacer_movimiento_jugador(Estado, Jugador, TipoMovimiento, NuevoEstado) :-

    arg(1, Estado, Tablero),

    between(1, 5, Y),
    arg(Y, Tablero, Fila),

    between(1, 5, X),
    
    arg(X, Fila, Jugador),

    X1 is X - 1,
    % % X1 > 0,
    X2 is X + 1,
    % X2 < 6,
    between(X1, X2, Xf),
    
    Y1 is Y - 1,
    % Y1 > 0,
    Y2 is Y + 1,
    % Y2 < 6,
    between(Y1, Y2, Yf),

    hacer_movimiento(Estado, Y ,X, Yf, Xf, TipoMovimiento, NuevoEstado).

% mejor_jugada([Movimiento|MovimientosPosibles], JugadorOriginal, Jugador, MinMax, Nivel, MejorJugada, MejorPuntaje).

tablero_final(0, Jugador, Estado, Valor) :-
    Suma = suma(0),
    contar_fichas(Estado, Jugador, Suma),
    arg(1, Suma, Res),
    Valor = Res.

tablero_final(_, Jugador, Estado, 1000) :-
    gano(Estado, Jugador).
    
tablero_final(_, Jugador, Estado, -1000) :-
    oponente(Jugador, Oponente),
    gano(Estado, Oponente).

tablero_final(_, _, Estado, 0) :-
    es_empate(Estado).


contar_fichas(Estado, Jugador, Suma) :-
    arg(1, Estado, Tablero),

    between(1, 5, Y),
    arg(Y, Tablero, Fila),

    between(1, 5, X),
    arg(X, Fila, Ficha),
    
    valor_ficha(Ficha, Jugador, Valor),

    arg(1, Suma, Res),
    ResActualizado is Res + Valor,
    nb_setarg(1, Suma, ResActualizado),

    fail.

contar_fichas(_, _, _).

valor_ficha(-, _, 0) :- !.

valor_ficha(X, X, 1) :- !.

valor_ficha(_, _, -1).
    
%% estado(m(f(x, o, -, -, -), f(-, -, -, -, -), f(-, -, -, -, -), f(-, -, -, -, -), f(-, -, -, -, -)), 0,0,0,0,2)
%% valor_tablero(1, x, estado(m(f(x, o, -, -, -), f(-, -, -, -, -), f(-, -, -, -, -), f(-, -, -, -, -), f(-, -, -, -, -)), 0,0,0,0,2), Valor)


%hacer_movimiento(estado(m(f(x, -, -, x, -), f(-, -, -, -, -), f(o, -, -, -, -), f(x, -, -, -, -), f(-, -, -, -, -)), 0,0,0,0,2), 1, 1, 2, 1, normal, Estado2)
% mejor_movimiento(estado(m(f(x, -, -, x, -), f(-, -, -, -, -), f(o, -, -, -, -), f(x, -, -, -, -), f(-, -, -, -, -)), 0,0,0,0,2), x, 2, ia_grupo, BestMove).

    
mejor_jugada(Nivel, JugadorOriginal, Jugador, MinMax, [Jugada| RestoJugadas], MejorJugada, MejorPuntaje) :-
    tablero_final(Nivel, JugadorOriginal, Jugada, Puntaje),
    mejor_jugada(Nivel, JugadorOriginal, Jugador, MinMax, RestoJugadas, MejorJugadaActual, MejorPuntajeActual),
    compararJugadas(MinMax, Jugada, Puntaje, MejorJugadaActual, MejorPuntajeActual, MejorJugada, MejorPuntaje).
    
mejor_jugada(Nivel, JugadorOriginal, Jugador, MinMax, [Jugada| RestoJugadas], MejorJugada, MejorPuntaje) :-
    mejor_jugada(Nivel, JugadorOriginal, Jugador, MinMax, RestoJugadas, MejorJugadaActual, MejorPuntajeActual),
    oponente(Jugador, Oponente),
    cambiarMinMax(MinMax, OtroMinMax),    
    mejor_movimiento_step(Jugada, JugadorOriginal, Oponente, Nivel, OtroMinMax, _, MejorPuntajeHoja),
    compararJugadas(MinMax, Jugada, MejorPuntajeHoja, MejorJugadaActual, MejorPuntajeActual, MejorJugada, MejorPuntaje).

mejor_jugada(_, _, _, max, [], [], -2000).

mejor_jugada(_, _, _, min, [], [], 2000).


compararJugadas(max, Jugada1, Puntaje1, _, Puntaje2, Jugada1, Puntaje1):-
        Puntaje1 >= Puntaje2, !.

compararJugadas(max, _, Puntaje1, Jugada2, Puntaje2, Jugada2, Puntaje2):-
        Puntaje1 < Puntaje2, !.

compararJugadas(min, Jugada1, Puntaje1, _, Puntaje2, Jugada1, Puntaje1):-
        Puntaje1 =< Puntaje2, !.

compararJugadas(min, _, Puntaje1, Jugada2, Puntaje2, Jugada2, Puntaje2):-
        Puntaje1 > Puntaje2, !.


%cambiarMinMax(+MinMax, -otroMinMax)
cambiarMinMax(min,max).
cambiarMinMax(max,min).


mejor_movimiento_etapa1(Estado, Jugador, _) :-
    arg(1, Estado, Tablero),
    between(1, 5, Y),
        arg(Y, Tablero, Fila),
        
        between(1, 5, X),
        \+ (X == 3,Y == 3),

        arg(X, Fila, -),
        setarg(X, Fila, Jugador).
        



    
   
    