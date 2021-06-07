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

oponente(o, x).
oponente(x, o).

indice_jugador(x, 0).
indice_jugador(o, 1).

gano(Estado, Jugador) :-
    oponente(Jugador, Oponente),
    indice_jugador(Oponente, Indice),
    IndiceTurnos is 2 + Indice,
    arg(IndiceTurnos, Estado, TurnosSinMover),
    TurnosSinMover >= 3.

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

numero_de_posibles_capturas(Estado, Jugador, Oponente, Cantidad) :-
    arg(1, Estado, Tablero),

    between(1, 5, Y),
    arg(Y, Tablero, Fila),

    between(1, 5, X),
    arg(X, Fila, Jugador),

    posibles_capturas(X, Y, Tablero, Jugador, Oponente, Cantidad).

numero_de_posibles_capturas(_, _, _, _).

tablero_final(0, Jugador, Estado, Valor) :-
    arg(6, Estado, 1), !,
    oponente(Jugador, Oponente),

    CapturasJugador = cant1(0),
    numero_de_posibles_capturas(Estado, Jugador, Oponente, CapturasJugador), 

    CapturasOponente = cant2(0),
    numero_de_posibles_capturas(Estado, Oponente, Jugador, CapturasOponente),

    arg(1, CapturasJugador, CapJugador),
    arg(1, CapturasOponente, CapOponente),

    Valor is CapJugador - CapOponente.

tablero_final(0, Jugador, Estado, Valor) :-
    Suma = suma(0),
    contar_fichas(Estado, Jugador, Suma),

    arg(1, Suma, Res),

    indice_jugador(Jugador, IndiceJ),
    IndiceJug is 2 + IndiceJ,
    arg(IndiceJug, Estado, JugSinMovJugador),

    oponente(Jugador, Oponente),
    indice_jugador(Oponente, IndiceO),
    IndiceOp is 2 + IndiceO,
    arg(IndiceOp, Estado, JugSinMovOponente),

    Valor is Res + JugSinMovOponente - JugSinMovJugador.

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

% comparar_jugadas(max, Jugada1, Puntaje1, _, Puntaje2, Jugada1, Puntaje1):-
%         Puntaje1 >= Puntaje2, !.

% comparar_jugadas(max, _, Puntaje1, Jugada2, Puntaje2, Jugada2, Puntaje2):-
%         Puntaje1 < Puntaje2, !.

% comparar_jugadas(min, Jugada1, Puntaje1, _, Puntaje2, Jugada1, Puntaje1):-
%         Puntaje1 =< Puntaje2, !.

% comparar_jugadas(min, _, Puntaje1, Jugada2, Puntaje2, Jugada2, Puntaje2):-
%         Puntaje1 > Puntaje2, !.


%cambiarMinMax(+MinMax, -otroMinMax)
% cambiarMinMax(min,max).
% cambiarMinMax(max,min).


% mejor_movimiento: dado un estado, un jugador, un nivel para minimax, y una estrategia, devuelve la mejor jugada posible
% Estrategia es solamente un átomo que se le asigna para poder implementar más de una estrategia
% mejor_movimiento(+Estado,+Jugador,+NivelMinimax,+Estrategia,-Estado2).
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
    % arg(6, Estado, 2),
    mejor_movimiento_step(Estado, Jugador, Jugador, Nivel, -10000, 10000,  MejorJugada, _).


%MEJOR MOVIMIENTO STEP
mejor_movimiento_step(Estado, JugadorOriginal, Jugador, Nivel, Alpha, Beta, MejorJugada, MejorPuntaje) :-
    Nivel > 0, !,
    Nivel1 is Nivel - 1,
    Alpha1 is - Beta,
    Beta1 is - Alpha,
    movimientos_posibles(Jugador, Estado, MovimientosPosibles),
    mejor_jugada(Nivel1, JugadorOriginal, Jugador, Alpha1, Beta1, MovimientosPosibles, nil, MejorJugada, MejorPuntaje).
    
mejor_movimiento_step(Estado, JugadorOriginal, _, _, _, _, _, MejorPuntaje) :-
    tablero_final(0, JugadorOriginal, Estado, MejorPuntaje).


% MEJOR JUGADA
mejor_jugada(Nivel, JugadorOriginal, Jugador, Alpha, Beta, [Jugada| RestoJugadas], JugadaAux, MejorJugada, MejorPuntaje) :-
    arg(6, Jugada, 2),
    tablero_final(Nivel, Jugador, Jugada, Puntaje), 

    cortar(Nivel, JugadorOriginal, Jugador, Alpha, Beta, Jugada, RestoJugadas, JugadaAux, MejorJugada, MejorPuntaje, Puntaje).
    
mejor_jugada(Nivel, JugadorOriginal, Jugador, Alpha, Beta, [Jugada| RestoJugadas], JugadaAux, MejorJugada, MejorPuntaje) :-

    oponente(Jugador, Oponente),
    mejor_movimiento_step(Jugada, JugadorOriginal, Oponente, Nivel, Alpha, Beta, _, MejorPuntajeHoja),

    MejorPuntajeHoja1 is -MejorPuntajeHoja,
    cortar(Nivel, JugadorOriginal, Jugador, Alpha, Beta, Jugada, RestoJugadas, JugadaAux, MejorJugada, MejorPuntaje, MejorPuntajeHoja1).

mejor_jugada(Nivel, _, _,Alpha, _, [], Jugada, Jugada, Alpha).
    % print(Nivel),write(' mejor_jugada2: '),print(Jugada),print(Alpha),write('\n').

% mejor_jugada(_, _, _, _, Beta, [], Jugada, Jugada, Beta).

cortar(_, _, _, _, Beta, Jugada, _, _, Jugada, MejorPuntajeHoja, MejorPuntajeHoja) :-
    % print(Jugada),
    % print(MejorPuntajeHoja), write('\n'),
    MejorPuntajeHoja >= Beta, !.

cortar(Nivel, JugadorOriginal, Jugador, Alpha, Beta, Jugada, RestoJugadas, _, MejorJugadaActual, MejorPuntajeActual, MejorPuntajeHoja) :-
    Alpha < MejorPuntajeHoja, MejorPuntajeHoja < Beta, !,
    mejor_jugada(Nivel, JugadorOriginal , Jugador, MejorPuntajeHoja, Beta, RestoJugadas, Jugada, MejorJugadaActual, MejorPuntajeActual).

cortar(Nivel, JugadorOriginal, Jugador, Alpha, Beta, _, RestoJugadas, JugadaAux, MejorJugadaActual, MejorPuntajeActual, MejorPuntajeHoja) :-
    MejorPuntajeHoja =< Alpha, !,
    mejor_jugada(Nivel, JugadorOriginal, Jugador, Alpha, Beta, RestoJugadas, JugadaAux, MejorJugadaActual, MejorPuntajeActual).


%MOVIMIENTOS POSIBLES
movimientos_posibles(Jugador, Estado, MovimientosPosibles) :-
    arg(6, Estado, 1),
    findall(NuevoEstado, hacer_movimiento_jugador_etapa1(Estado, Jugador, NuevoEstado), MovimientosPosibles),
    \+ (MovimientosPosibles == []), !.

movimientos_posibles(Jugador, Estado, MovimientosPosibles) :-
    arg(6, Estado, 1), !,
    setarg(6, Estado, 2),
    findall(NuevoEstado, hacer_movimiento_jugador(Estado, Jugador, normal, NuevoEstado), MovimientosPosibles),
    setarg(6, Estado, 1).
    
movimientos_posibles(Jugador, Estado, MovimientosPosibles) :-
    findall(NuevoEstado, hacer_movimiento_jugador(Estado, Jugador, normal, NuevoEstado), MovimientosPosibles).

hacer_movimiento_jugador_etapa1(Estado, Jugador, NuevoEstado) :-
    arg(1, Estado, Tablero),

    between(1, 5, Y1),
    arg(Y1, Tablero, Fila1),

    between(1, 5, X1),
    \+ (X1 == 3, Y1 == 3),

    arg(X1, Fila1, -),

    setarg(X1, Fila1, Jugador),

    between(Y1, 5, Y2),
    arg(Y2, Tablero, Fila2),

    between(1, 5, X2),
    \+ (X2 == 3, Y2 == 3),

    arg(X2, Fila2, -),

    setarg(X2, Fila2, Jugador),

    NuevoEstado = Estado.
        
%MOVIMIENTO JUGADOR
hacer_movimiento_jugador(Estado, Jugador, normal, NuevoEstado) :-
    \+ hay_movimiento(Estado, Jugador), !,

    indice_jugador(Jugador, Indice),
    IndiceJug is 2 + Indice,

    arg(IndiceJug, Estado, JugSinMover),
    JugSinMoverAct is JugSinMover + 1,

    setarg(IndiceJug, Estado, JugSinMoverAct),
    
    NuevoEstado = Estado.

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


% Hace el movimiento a partir del Estado inicial del tablero, las coordenadas origen y destino y el tipo de movimiento
% el tipo de movimiento puede ser normal o con_captura. En el segundo caso, el movimiento debe incluir una captura, o fallar.
% Tablero y Tablero2 tiene el mismo término, solamente se utiliza para poder consultar la variable de salida desde el bridge
% hacer_movimiento(+Tablero, +FilaOrigen,+ColumnaOrigen,+FilaDestino,+ColumnaDestino,+TipoMovimiento,-Estado2).

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


%HAY POSIBLE CAPTURA
hay_posible_captura(Estado, Jugador) :- 
    oponente(Jugador, Oponente),
    arg(1, Estado, Tablero),
    between(1, 5, Y),
    arg(Y, Tablero, Fila), 

    between(1, 5, X),
    arg(X, Fila, Jugador_Aux),

    Jugador == Jugador_Aux,
    
    hay_mov_cap_ficha(X, Y, Tablero, Jugador, Oponente).

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


mejor_movimiento_etapa1(Estado, Jugador, _) :-
    arg(1, Estado, Tablero),
    between(1, 5, Y),
        arg(Y, Tablero, Fila),
        
        between(1, 5, X),
        \+ (X == 3,Y == 3),

        arg(X, Fila, -),
        setarg(X, Fila, Jugador).


posibles_capturas(X, Y, Tablero, Jugador, Oponente, Capturas) :-
    % Captura hacia la derecha simple
    X3 is X + 3,
    \+ X3 > 5,
    arg(Y, Tablero, Fila),

    arg(X3, Fila, Jugador),

    X2 is X + 2,
    arg(X2, Fila, Oponente),

    arg(1, Capturas, Suma),
    SumaAct is Suma + 1,
    nb_setarg(1, Capturas, SumaAct),
    fail;
    % Captura hacia la izquierda simple
    X3 is X - 3,
    \+ X3 < 1,
    arg(Y, Tablero, Fila),

    arg(X3, Fila, Jugador),
    X2 is X - 2,
    arg(X2, Fila, Oponente),

    arg(1, Capturas, Suma),
    SumaAct is Suma + 1,
    nb_setarg(1, Capturas, SumaAct),
    fail;
    % Captura hacia arriba simple
    Y3 is Y - 3,
    \+ Y3 < 0,
    arg(Y3, Tablero, FilaJ),
    arg(X, FilaJ, Jugador),

    Y2 is Y - 2,
    arg(Y2, Tablero, FilaO),
    arg(X, FilaO, Oponente),

    arg(1, Capturas, Suma),
    SumaAct is Suma + 1,
    nb_setarg(1, Capturas, SumaAct),
    fail;
    % Captura hacia abajo simple
    Y3 is Y + 3,
    \+ Y3 > 5,
    arg(Y3, Tablero, FilaJ),
    arg(X, FilaJ, Jugador),

    Y2 is Y + 2,
    arg(Y2, Tablero, FilaO),
    arg(X, FilaO, Oponente),

    arg(1, Capturas, Suma),
    SumaAct is Suma + 1,
    nb_setarg(1, Capturas, SumaAct),
    fail;
    % Captura hacia arriba con movimiento a la izquierda
    Y2 is Y - 2,
    Y2 > 0,
    X1 is X - 1,
    X1 > 0,
    
    arg(Y2, Tablero, FilaJ),
    arg(X1, FilaJ, Jugador),

    Y1 is Y - 1,
    arg(Y1, Tablero, FilaO),
    arg(X1, FilaO, Oponente),

    arg(1, Capturas, Suma),
    SumaAct is Suma + 1,
    nb_setarg(1, Capturas, SumaAct),
    fail;
    
    % Captura hacia abajo con movimiento a la izquierda
    Y2 is Y + 2,
    Y2 < 6,
    X1 is X - 1,
    X1 > 0,
    
    arg(Y2, Tablero, FilaJ),
    arg(X1, FilaJ, Jugador),

    Y1 is Y + 1,
    arg(Y1, Tablero, FilaO),
    arg(X1, FilaO, Oponente),

    arg(1, Capturas, Suma),
    SumaAct is Suma + 1,
    nb_setarg(1, Capturas, SumaAct),
    fail;

    % Captura hacia la izquierda con movimiento hacia arriba
    X2 is X - 2,
    X2 > 0,
    Y1 is Y - 1,
    Y1 > 0,
    
    arg(Y1, Tablero, Fila),
    arg(X2, Fila, Jugador),

    X1 is X - 1,
    arg(X1, Fila, Oponente),

    arg(1, Capturas, Suma),
    SumaAct is Suma + 1,
    nb_setarg(1, Capturas, SumaAct),
    fail;

    % Captura hacia la derecha con movimiento hacia arriba
    X2 is X + 2,
    X2 < 6,
    Y1 is Y - 1,
    Y1 > 0,
    
    arg(Y1, Tablero, Fila),
    arg(X2, Fila, Jugador),

    X1 is X + 1,
    arg(X1, Fila, Oponente),

    arg(1, Capturas, Suma),
    SumaAct is Suma + 1,
    nb_setarg(1, Capturas, SumaAct),
    fail;

    %Captura hacia arriba con movimiento a la derecha 
    Y2 is Y - 2,
    Y2 > 0,
    X1 is X + 1,
    X1 < 6,
    
    arg(Y2, Tablero, FilaJ),
    arg(X1, FilaJ, Jugador),

    Y1 is Y - 1,
    arg(Y1, Tablero, FilaO),
    arg(X1, FilaO, Oponente),
    
    arg(1, Capturas, Suma),
    SumaAct is Suma + 1,
    nb_setarg(1, Capturas, SumaAct),
    fail;

    % Captura hacia abajo con movimiento a la derecha
    Y2 is Y + 2,
    Y2 < 6,
    X1 is X + 1,
    X1 < 6,
    
    arg(Y2, Tablero, FilaJ),
    arg(X1, FilaJ, Jugador),

    Y1 is Y + 1,
    arg(Y1, Tablero, FilaO),
    arg(X1, FilaO, Oponente),

    arg(1, Capturas, Suma),
    SumaAct is Suma + 1,
    nb_setarg(1, Capturas, SumaAct),
    fail;

    % Captura hacia la izquierda con movimiento hacia abajo
    X2 is X - 2,
    X2 > 0,
    Y1 is Y + 1,
    Y1 < 6,

    arg(Y1, Tablero, Fila),
    arg(X2, Fila, Jugador),

    X1 is X - 1,
    
    arg(X1, Fila, Oponente),

    arg(1, Capturas, Suma),
    SumaAct is Suma + 1,
    nb_setarg(1, Capturas, SumaAct),
    fail;

    % Captura hacia la derecha con movimiento hacia abajo
    X2 is X + 2,
    X2 < 6,
    Y1 is Y + 1,
    Y1 < 6,

    arg(Y1, Tablero, Fila),
    arg(X2, Fila, Jugador),

    X1 is X + 1,
    
    arg(X1, Fila, Oponente),

    arg(1, Capturas, Suma),
    SumaAct is Suma + 1,
    nb_setarg(1, Capturas, SumaAct),
    fail.

posibles_capturas(_, _, _, _, _, _).


