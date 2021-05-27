# -*- coding: utf-8 -*-
"""
Python function to bridge between the server and the Prolog logic.
"""
from os import path
from typing import List, Optional
from itertools import islice
import re

from pyswip_mt import PrologMT
from jinja2 import Template

BASE_PROLOG_FILE = "arabelog.pl"
STACK_LIMIT = 4000000000    # Limit to about 30 seconds runtime
OTHER_PLAYER_SYMBOL = lambda x: "x" if x == "O" else "o"

prolog = PrologMT()
currently_consulted = ""


# Esta función es la que levanta el tablero
def consultar_reglas_juego() -> None:

    # Enlarge stack
    archivo_reglas= f"{BASE_PROLOG_FILE}"
    # Check if file matches current one being used
    global currently_consulted
    if archivo_reglas == currently_consulted:
        return

    next(prolog.query(f"set_prolog_flag(stack_limit, {STACK_LIMIT})."))
    next(prolog.query(f'unload_file("{currently_consulted}").'))
    prolog.consult(archivo_reglas)
    currently_consulted = archivo_reglas


# Recibe un tablero que es una lista de listas
# Y retorna un string en el formato que Prolog espera
def tablero_to_prolog(tablero: List[List], datos_estado:List[int], jugador_actual) -> str:
    """
    Convertimos el tablero del objeto Python (lista de listas) al formato que espera Prolog. 
    También incluimos los 4 valores del Estado: las jugadas que tienen ambos sin capturar y
    la cantidad de veces seguidas que no hay podido jugar cada uno. 
    Ejemplo: 
    tablero: [['', '', '', 'x', ''],['', '', '', 'x', ''],['', '', '', 'o', ''],['', 'x', 'o', 'x', 'o'],['', '', '', 'x', '']]
    datos_estado: [1,0,5,4,2]
    Devuelve: 'estado(m(f(-,-,-,x,-),f(-,-,-,x,-),f(-,-,-,o,-),f(-,x,o,x,o),f(-,-,-,x,-)),1,0,5,4,2)'

    Si la celda vale '-', entonces le pongo el valor del jugador actual
    """
    
    tablero_str = 'm('
    for fila in tablero:
        tablero_str +='f('
        for celda in fila:
            if celda=='-':
                tablero_str+=jugador_actual.lower()+','
            elif celda:
                tablero_str+=celda.lower()+','
            else:
                tablero_str+='-,'
        tablero_str = tablero_str[:-1]
        tablero_str +='),'
    tablero_str = tablero_str[:-1]
    tablero_str += ')'

    tablero_prolog = 'estado('+ tablero_str + ','+str(datos_estado[0])+','+str(datos_estado[1])+','+str(datos_estado[2])+ ','+ str(datos_estado[3])+ ','+ str(datos_estado[4])+')'
    return tablero_prolog


# Recibe un string con el formato Prolog y devuelve Lista de Listas
# Los '-' se transforman en strings vacíos
def prolog_to_tablero(tablero_str: str) -> List[List]:
    """
    Convierte el objeto Prolog a una lista de listas de Python. Asume que el string es sintácticamente válido
    El ejemplo es exactamente el mismo que en tablero_to_prolog, pero al revés
    """
    
    # Primero obtengo los 4 parámetros del estado
    r=re.search('estado\((m\(.*\),(.{1,2}),(.{1,2}),(.{1,2}),(.{1,2}),(.{1,2}))\)', tablero_str.replace(" ",""))
    estado=[int(r.group(2)),int(r.group(3)),int(r.group(4)),int(r.group(5)), int(r.group(6))]

    tablero_str=r.group(1)

    # Elimino todo lo que no sean símbolos de celda
    # Queda un string con 25 símbolos
    piezas=re.sub(r'[mf(), ]', '', tablero_str)
    
    # Como el tablero tiene largo fijo, no me complico la vida para transformarlo a una lista de listas
    tablero=[[],[],[],[],[]]
    tablero[0]=list(islice(piezas,0,5))
    tablero[1]=list(islice(piezas,5,10))
    tablero[2]=list(islice(piezas,10,15))
    tablero[3]=list(islice(piezas,15,20))
    tablero[4]=list(islice(piezas,20,25))
     
    # Cambio los - por strings vacíos
    # Tiene que haber una forma más pythonic de hacer esto, pero ta. 
    tablero_retorno = tablero
    for fila in range(5):
        for columna in range(5): 
            if tablero[fila][columna]=='-':
                tablero_retorno[fila][columna]=''
            elif tablero[fila][columna]=='x':
                tablero_retorno[fila][columna]='X'
            else:
                tablero_retorno[fila][columna]='O'

    
    return tablero_retorno



# Recibe el tablero, el estado y un jugador al que le toca mover, así como la estrategia a utilizar
# Devuelve un nuevo tablero y un nuevo estado, utilizando minimax
def do_mejor_movimiento(tablero:List[List],datos_estado:List[int],jugador:int,nivel_minimax:int,estrategia:str) ->List[List]:
    consultar_reglas_juego()
    tablero_prolog = tablero_to_prolog(tablero,datos_estado,jugador)
 
    if jugador=='O':
        jugador_consulta='o'
    else:
        jugador_consulta='x'
    prolog_query =  f"mejor_movimiento({tablero_prolog},{jugador_consulta},{nivel_minimax},{estrategia},Estado2)."

    print(prolog_query)
    query_result = list(prolog.query(prolog_query, maxresult=1))
    if (query_result == []):
        tablero = []
    else:
        tablero= prolog_to_tablero(query_result[0].get("Estado2"))

    return tablero


# Chequea que un movimiento sea válido y lo ejecuta
# Si el movimiento no es válido, devuelve una lista vacía
# De lo contrario, devuelve el nuevo tablero
# Recibe el tablero, los parámetros de estado, el jugador actual, y el origen y destino del movimiento
# Si debe capturar = True, entonces el movimiento debe incluir una captura, porque de lo contrario no es válido
def do_ejecutar_movimiento(tablero:List[List],datos_estado:List[int], jugador_actual:int,fila_origen:int, columna_origen:int, fila_destino:int, columna_destino:int, debe_capturar:bool) ->List[List]:
    consultar_reglas_juego()
    estado_prolog = tablero_to_prolog(tablero,datos_estado,jugador_actual)
    
    # Elijo el tipo de movimiento antes de invocar al predicado
    if (debe_capturar):
        tipo_movimiento='con_captura'
    else:
        tipo_movimiento='normal'

    prolog_query =  f"hacer_movimiento({estado_prolog},{fila_origen},{columna_origen},{fila_destino},{columna_destino},{tipo_movimiento},Tablero2)."
    print(prolog_query)
    query_result = list(prolog.query(prolog_query, maxresult=1))
    if (query_result == []):
        tablero = [],
        nuevos_datos_estado=datos_estado
    else:
        tablero= prolog_to_tablero(query_result[0].get("Tablero2") )
        
    return tablero


# Retorna True si existe un movimiento que pueda hacer una captura
def do_hay_posible_captura(tablero:List[List],datos_estado:List[int], jugador_actual:str) -> Optional[bool]:
    consultar_reglas_juego()
    tablero_prolog = tablero_to_prolog(tablero,datos_estado,jugador_actual)
    #consult_board_size(5)

    if jugador_actual=='O':
        jugador_consulta='o'
    else:
        jugador_consulta='x'

    prolog_query =  f"hay_posible_captura({tablero_prolog},{jugador_consulta})."
    print(prolog_query)
    query_result = list(prolog.query(prolog_query, maxresult=1))
    print(query_result)
    if len(query_result) > 0:
        return True
    else:
        return False

# Retorna True si existe un movimiento que el jugador pueda hacer
def do_hay_movimiento(tablero:List[List],datos_estado:List[int], jugador_actual:str) -> Optional[bool]:
    consultar_reglas_juego()
    tablero_prolog = tablero_to_prolog(tablero,datos_estado,jugador_actual)
    #consult_board_size(5)

    if jugador_actual=='O':
        jugador_consulta='o'
    else:
        jugador_consulta='x'

    prolog_query =  f"hay_movimiento({tablero_prolog},{jugador_consulta})."
    print(prolog_query)
    query_result = list(prolog.query(prolog_query, maxresult=1))
    if len(query_result) > 0:
        return True
    else:
        return False

