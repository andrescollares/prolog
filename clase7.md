# Fundamentos teóricos

# Lógica de primer orden

## Sintaxis

### Alfabeto de 1er orden

Consituido por:
 - variables (x, y, z, ...)

 - constantes (a, b, c, ...)

 - functores (f, g, h, ...)

 - símbolos de predicados (p, q, r, ...)

 - conectivos: $\neg, \to, \leftrightarrow, \wedge, \vee$

 - cuantificadores: $\forall, \exists$

 - puntuación: ')', '(', ','

### Término
 1. Una variable es un término

 1. Una constante es un término.

 1. Si f es una función n-aria y t1 ... tn términos, f(t1, ..., tn) es un término.

 1. Estos son todos los terminos

### Fórmula bien formada

 1. Si p es un predicado n-ario y t1 ... tn términos, p(t1, ..., tn) es una fórmula (átomo o fórmula atómica).

 1. Si F y G son fórmulas, también lo son:
    - ($\neg$F)

    - (F $\vee$ G)

    - (F $\wedge$ G)

    - (F $\to$ G)

    - (F $\leftrightarrow$ G)
 
 1. Si F es una fórmula y x una variable, también son fórmulas:
    - ($\exists$ x F)

    - ($\forall$ x F)

 1. Estas son todas las fórmulas.

### Lenguaje de primer orden

Es el conjunto de fórmulas bien formadas que se pueden construir con los símbolos del alfabeto.

### Alcance de un cuantificador

El alcance de $\exists$ en ($\exists$ x F) es F;
el alcance de $\forall$ en ($\forall$ x F) es F

### Variables ligadas/libres

Una ocurrencia de una variable está ligada si está inmediatamente luego de un cuantificador o es una ocurrencia dentro del alcance de un cuantificador.

En caso contrario, diremos que la ocurrencia se encuentra libre.

Se dice que la variables es una variable ligada (libre) cuando todas sus ocurrencias están ligadas (libres).

### Fórmula cerrada

Una fórmula cerrada es una fórmula que no presenta ocurrencias de variables libres.

> Los teoremas matemáticos y las clausulas de un problema lágico son formulas cerradas, se utilizan para hacer demostraciones. Son formulas que toman algun valor de verdad según la interpretación

### Clausura universal (existencial)

$\forall$(F) es la **clausura universal** de F, y se obtiene agregando un cuantificador universal por cada variable con una ocurrencia libre en F.

Análogamente, la **clausura existencial** $\exists$(F) se obtiene agregando un cuantificador existencial por cada variable con una ocurrencia libre en F.

### Literal

Un *literal* es un átomo (literal positivo) o su negado (literal negativo).

### Cláusula

Una cláusula es una fórmula cerrada con la siguiente forma:
$$\forall x_1 ...~x_k (L_1 \vee ... \vee L_p)$$

en donde cada $L_i$ es un literal y $x_1 ...~x_k$ son todas las variables que ocurren en $L_1 \vee ... \vee L_p$

Se utilizaran indistintamente las siguientes notaciones para las cláusulas:
 $$\forall x_1 ...~x_k (A_1 \vee ... \vee A_p \vee \neg B_1 \vee ... \vee \neg B_q)$$

 $$(A_1 \vee ... \vee A_p \leftarrow B_1 \wedge ... \wedge B_q)$$

 $$\{A_1, ..., A_p, \neg B_1, ..., \neg B_q\}$$

### Cláusula definida

Una cláusula definida es una cláusula que contiene exactamente un literal positivo.

 - regla: $A_1 \leftarrow B_1 \wedge ... \wedge B_q$
 - cláusula unitaria o hecho: $A_1 \leftarrow $

### Programa definido

Un programa definido es un conjunto de cláusulas definidas (reglas y hechos).

### Objetivo definido

Un objetivo definido es una cláusula sin literales positivos:
$$\leftarrow B_1, ..., B_q$$

>Esto es la consulta

### Cláusula de Horn

Una cláusula de Horn es una cláusula definida o un objetivo definido.

Tanto los programas lógicos (conjuntos de cláusulas definidas) como las consultas van a ser modeladas con cláusulas de Horn.

Las consultas van a ser negadas para así obtener objetivos definidos.

## Semántica

Para decir que una fórmula es verdadera o falsa es necesario darles algún significado a los símbolos en la fórmula.

### Interpretación

Sea $L$ un lenguaje de 1er orden.

Una interpretación $I$ para $L$ consiste en :

 - Un conjunto $D$ no vacío (dominio de la interpretación)

 - Un mapeo $M_c$ de las constantes de $L$ en $D$.

 - Un mapeo $M_f$ para los símbolos de función de $L$, tal que a $f$ n-ario le corresponde una función $D_n \to D$

 - Un mapeo $M_p$ para los símbolos de predicado de $L$, tal que a $p$ n-ario le corresponde una función $D_n \to \{v, f\}$ (es decir, una relación en $D_n$
 
### Valor de una fórmula $F$ según una interpretación $I$ y una asignacion $V$

#### Atómica

1. A cada variable se le da un valor $h(v)$ según V.

1. A cada constante c se le da un valor $h(c)$ según I, $h(c) = M_c(c)$.

1. El valor asociado a un término $f(t1, ..., tn)$ es $h(f(t1, ..., tn) = M_f(h(t1), ..., h(tn))$ ($h(f)$ en $D$).

1. El valor de verdad asociado a una fórmula atómica $p(t1, ..., tn)$ es $M_p(h(t1), ..., h(tn))$.

#### Conectivos

$F$ es de la forma:

$$F1 \wedge F2$$

$$F1 \vee F2$$

$$F1 \to F2$$

$$\neg F1$$

En este caso se utiliza la tabla de verdad de los conectivos:

![](imagenes/tabla-conectivos.png)
 
#### Cuantificadores

$F$ es de la forma:

$$\exists x~P(x)$$

$$\forall x~P(x)$$

 1. $F$ es verdadera si existe $d \in D$, tal que $G$ es verdadera para $I$ y $V (x/d)$, donde $V (x/d)$ es $V$ excepto para la variable $x$, a la que se le asigna el valor $d$; en caso contrario es falsa.

 1. $F$ es verdadera si para cada $d \in D$, $G$ es verdadera para $I$ y $V (x/d)$; en caso contraria es falsa.

# Modelos

## Definición

Sea $F$ una fórmula cerrada e $I$ una interpretación, $I$ es un **modelo** para $F$ si $F$ es verdadera según $I$ ($I$ satisface $F$).

Sea $S$ un conjunto de fórmulas cerradas, $I$ es un modelo para $S$ si satisface todas las fórmulas de $S$.

## Semántica

### Satisfactible/Válido

Sea $S$ un conunto de fórmulas cerradas

 - $S$ es **satisfactible** si extiste al menos un modelo para $S$.

 - $S$ es **válido** (lógicamente valido) si toda interpretación es modelo.

 - $S$ es **insatisfactible** si ninguna interpretación es modelo.

### Consecuencia lógica

Sea $S$ un conjunto de fórmulas cerradas y $F$ una fórmula cerrada:

$F$ es **consecuencia lógica** de $S$ si todo modelo de $S$ es modelo de $F$. ($S \models F$)

#### Teorema

Sea $S = \{F_1, F_2, ..., F_n\}$, $F$ es consecuencia lógica de S sii $(F_1 \wedge F_2 \wedge ... \wedge F_n) \to F$ es una fórmula válida.

#### Teorema

Sea $S$ un conjunto de formulas cerradas y $F$ una fórmula cerrada sobre $L$ de primer orden:
$$ S \models F~\text{ sii }~S \cup \{\neg F\} \text{ es insatisfactible.}$$ 

##### Demostración

<details>
  <summary>Expandir</summary>

  ![](imagenes/dem-teo-cons1.png)
  ![](imagenes/dem-teo-cons2.png)
</details>

# Pruebas en prolog

Para probar que una consulta $C$ es consecuencia lógica de un programa lógico $P$ basta con probar que $ P \cup \{\neg C\} $ es insatisfactible.

A este tipo de demostración se le llama **prueba por refutación**: se niega lo que se quiere probar, llegando a un absurdo.

Debemos buscar una forma automática de resolver este problema, para esto se introducen los **métodos de resolución**.

## Resolución

