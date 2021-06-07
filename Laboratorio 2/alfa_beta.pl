%MEJOR MOVIMIENTO STEP
mejor_movimiento_step(Estado, JugadorOriginal, Jugador, Nivel, Alpha, Beta, MejorJugada, MejorPuntaje) :-
    Nivel > 0, !,
    Alpha1 is -Beta,
    Beta1 is -Alpha,
    Nivel1 is Nivel - 1,
    movimientos_posibles(Jugador, Estado, MovimientosPosibles),
    mejor_jugada(Nivel1, JugadorOriginal, Jugador, Alpha1, Beta1, MovimientosPosibles, nil, MejorJugada, MejorPuntaje).
    
mejor_movimiento_step(Estado, JugadorOriginal, _, _, _, _, _, MejorPuntaje) :-
    tablero_final(0, JugadorOriginal, Estado, MejorPuntaje).


% MEJOR JUGADA
mejor_jugada(Nivel, JugadorOriginal, Jugador, Alpha, Beta, [Jugada| RestoJugadas], JugadaAux, MejorJugada, MejorPuntaje) :-
    tablero_final(Nivel, JugadorOriginal, Jugada, Puntaje),

    cortar(Nivel, JugadorOriginal, Jugador, Alpha, Beta, Jugada, RestoJugadas, JugadaAux, MejorJugada, MejorPuntaje, Puntaje), print(Nivel), print(Alpha), print(Beta), write('\n').
    
mejor_jugada(Nivel, JugadorOriginal, Jugador, Alpha, Beta, [Jugada| RestoJugadas], JugadaAux, MejorJugada, MejorPuntaje) :-

    oponente(Jugador, Oponente),
    mejor_movimiento_step(Jugada, JugadorOriginal, Oponente, Nivel, Alpha, Beta, _, MejorPuntajeHoja),
    MejorPuntajeHoja1 is -MejorPuntajeHoja,
    cortar(Nivel, JugadorOriginal, Jugador, Alpha, Beta, Jugada, RestoJugadas, JugadaAux, MejorJugada, MejorPuntaje, MejorPuntajeHoja1), print(Nivel), print(Alpha), print(Beta), write('\n').

% mejor_jugada(_, _, _, Alpha, [], [], -2000).

% mejor_jugada(_, _, _, min, [], [], 2000).

mejor_jugada(Nivel, _, _,Alpha, Beta, [], Jugada, Jugada, Alpha) :- print(Nivel), print(Alpha), print(Beta), write('\n').

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

