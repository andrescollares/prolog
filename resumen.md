# RESUMEN LABORATORIO

## Cut

#### verde

```prolog
max(X, Y, Y) :- X =< Y, !.
max(X, Y, X) :- X > Y.
```

#### if_then_else

```prolog
if_then_else(P, Q, _R) :- P, !, Q.
if_then_else(P, _Q, R) :- R.
```

## Negación por falla (Not)

    \+

```prolog
not(X) :- X, !, fail.
not(X).
```
X es un predicado.

- Costoso
- Lo mejor es llamarlo cuando ya tenemos instanciada la variable que queremos verificar

#### ejemplos
```prolog
disjuntos(X,Y) :- \+(member(T,X), member(T,Y)).
```

```prolog
dif([], _, []).
dif([X|Xs], Ys, [X, Zs]) :- 
    \+ member(X, Ys), !, 
    dif(Xs, Ys, Z).
dif([X|Xs, Ys, Zs) :- 
    %member(X, Ys)
    dif(Xs, Ys, Zs).
```

## findall

```prolog
findall(+Template,+Goal,-Bag)
```
Bag es la lista con todas las instancias de Template para las cuales se satisface Goal. En caso de que Goal sea insatisfactible, Bag unifica con la lista vacía.

#### ejemplo
```prolog
ancestro(juan,jose). 
ancestro(juan,ana). 
ancestro(jose,pedro). 
ancestro(laura,ana). 
ancestro(lucia,jose). 

?- findall(X,ancestro(X,Y),L). 
L=[juan, juan, jose, laura, lucia].

?- findall([X,Y],ancestro(X,Y),Z).
Z = [[juan,jose], [juan,ana], [jose,pedro], [laura,ana], [lucia,jose]].
```

```prolog
interseccion(C1,C2,C3) :- findall(X, (member(X,C1), member(X,C2)), C3).
```

## bagof
```prolog
bagof(+Template,+Goal,-Bag)
```
Bag es la lista con todas las instancias de Template para las cuales se satisface Goal para una “asignación” de las variables que NO aparecen en Template. En caso de que Goal no sea satisfactible, falla.

#### ejemplo
```prolog
ancestro(juan,ana). 
ancestro(jose,pedro). 
ancestro(laura,ana). 
ancestro(lucia,jose). 

?- bagof(Y,ancestro(X,Y),L). 

X = jose, 
L = [pedro]; 

X = juan, 
L = [jose, ana]; 
```

## setof
Ídem a bagof, pero con Bag ordenada y sin repetidos.

## functor
```prolog
functor(Termino,Functor,Aridad)
```

```prolog
functor(padre(juan,jose),F,A).

F = padre,
A = 2
```

## arg
```prolog
arg(N,Termino,Argumento)
```

```prolog
arg(2, padre(juan, Y), manuel).

Y = manuel.
```

## nb_setarg
```prolog
nb_setarg(+N,+Term,+Value)
```
Setea el N-ésimo argumento del término Term como Value

## univ
```prolog
Termino =.. Lista
```
Permite construir e inspeccionar términos n-arios

```prolog
Termino =.. [func,arg1,arg2,arg3].
Termino = func(arg1,arg2,arg3).
```


## ciclo loop-fail

Primero un objetivo a cumplir, y luego falla.

#### ejemplo
imprimir los números de 1 a X

```prolog
imprimir(X):-
    between(1,X,I),
    writeln(I),
    fail.
imprimir(_).
```


```prolog
custom_findall(Template, Goal, Bag) :-
    Res = res([]),
    custom_findall2(Template, Goal, Res).
    Res = res(Bag).

custom_findall2(Template, Goal, Res) :-
    Goal,
    arg(1, Res, L),
    nb_setarg(1, Res, [Template|L],
    fail.

custom_findall2(_,_,_).
```


