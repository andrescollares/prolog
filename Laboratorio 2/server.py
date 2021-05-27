from flask import Flask, jsonify, request

from prolog_bridge import do_ejecutar_movimiento, do_hay_posible_captura, do_hay_movimiento, do_mejor_movimiento

app = Flask(__name__)


@app.route("/")
def index():
    """
    Index page of server, returns index.html.
    """
    with open("index.html", "r") as f_obj:
        return f_obj.read()



@app.route("/api/ejecutar_movimiento", methods=["POST"])
def ejecutar_movimiento():
    """"
    Le paso el tablero y las posiciones y ejecuta el movimiento
    Incluye validar que el movimiento solicitado sea válido
    Cambiar la ficha de lugar
    Y finalmente capturar las piezas correspondientes
    Devuelve un tablero vacío si falla, y el nuevo tablero en caso contrario
    """
    data = request.get_json()
    nuevo_tablero = do_ejecutar_movimiento(data.get("tablero"),
                                data.get("datos_estado"),
                                data.get("jugador_actual"),
                                data.get("fila_origen"),
                                data.get("columna_origen"),
                                data.get("fila_destino"), 
                                data.get("columna_destino"),
                                data.get("debe_capturar"))
    return jsonify({"tablero":nuevo_tablero})


@app.route("/api/hay_posible_captura", methods=["POST"])
def hay_posible_captura():
    """"
    Recibe el tablero y un jugador al que le toca mover
    Y devuelve True si el jugador tiene algún movimiento que permita capturar fichas.
    De lo contrario, devuelve False
    """
    data = request.get_json()
    result = do_hay_posible_captura(data.get("tablero"),data.get("datos_estado"), data.get("jugador"))

    return jsonify({"result":result})

@app.route("/api/hay_movimiento", methods=["POST"])
def hay_movimiento():
    """"    
    Recibe el tablero y un jugador al que le toca mover
    Y devuelve True si el jugador tiene algún movimiento
    De lo contrario, devuelve False
    """
    data = request.get_json()
    result = do_hay_movimiento(data.get("tablero"), data.get("datos_estado"),data.get("jugador"))

    return jsonify({"result":result})

@app.route("/api/mejor_movimientoIA", methods=["POST"])
def mejor_movimiento():
    """"    
    Recibe el tablero y un jugador al que le toca mover
    Y devuelve True si el jugador tiene algún movimiento
    De lo contrario, devuelve False
    """
    data = request.get_json()
    tablero = do_mejor_movimiento(data.get("tablero"), data.get("datos_estado"), data.get("jugador"),data.get("nivel_minimax"),data.get("estrategia"))

    return jsonify({"tablero":tablero})

if __name__ == "__main__":
    app.run(debug=True, host="0.0.0.0")
