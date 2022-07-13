# Sudoku Hidato
## Segundo Proyecto Programación Declarativa 2022

### Integrates:
- Daniel Alejandro Cárdenas Cabrera C-311
- Javier Alejandro Oramas López C-312

## Estructuras
Para la implementación del proyecto se usaron 2 estructuras principales Board y Node

### Board 
Representa el tablero del juego, conteniendo todas las casillas del sudoku
sus atributos son: 
- cells: Lista de [Node] que representa las casillas que conforman el tablero,
- minNum: Int, mínimo número presente en el tablero (este es el punto prefijado inicial, necesario en todos los tableros),
- maxNum: Int, máximo número presente en el tablero (este es el punto prefijado final, necesario en todos los tableros),

La representación recomendada para los tableros es generar un tablero cuadrado/rectangular que contenga los nodos que no se desea que 
sean parte del tablero con valor -1, esto hará que sean descartados pero a la hora de imprimir el tablero este se mostrará correctamente

### Node 
Representa una casilla del tablero, con su valor y posición
sus atributos son:
- value: Int, Valor contenido en la casilla
- position: tupla (Int,Int), Representa las posiciones x,y de la casilla en cuestión

Los Node con value (-1) se asumen como casillas que no son válidas y seran ignoradas

## Resolviendo Tableros

Para la solución de un tablero se optó por un algoritmo de backtracking, cuyo caso base será que todos los números fueron colocados en el tablero y el último número colocado es adyacente a maxNum (condición necesaria pues es posible colocarlos todos sin que sea un tablero válido aún cuando hasta el penúltimo número se cumple la correctitud de la solución)

En cada llamado recursivo se tiene Positions que será una lista con las posiciones de todas las casillas vacías adyacentes a la casilla actual, esta lista se le pasa a la función recursiveCall. 

Dicha función revisa que si el sucesor del valor del nodo actual es uno de los nodos prefijados en el tablero, y, de serlo, revisa si es adyacente al nodo actual.
Si el sucesor está y es adyacente, se salta a este nodo y se continúa a partir de este nodo.
Si no es valida la solución que se está construyendo, se elimina el último elemento de la lista y se continúa ejecutando recursiveCall
Se van probando todas las posiciones en cada uno de los adyacentes vacíios al nodo actual en caso de que su sucesor no esté en el tablero

Finalmente se retorna el tablero con las posiciones llenas

## Generando Tableros

El generador de tableros implementado genera tableros rectangulares, recibiendo la cantidad de filas y columnas, el mínimo valor y el máximo, ambos a ser prefijados en el tablero a generar.

Inicialmente crea los nodos inicial y final, creando aleatoriamente las posiciones de dichos nodos.

Luego genera un tablero rectangular completamente vacío, sustituye los nodos anteriormente creados por los vacíos en el tablero.
Una vez hecho esto, llena el tablero vacío con una solución válida, a dicho tablero se le eliminan los valores de la mitad -1 de los nodos
de forma tal que se garantice la unicidad de la solución y se retorna dicho tablero.


## Ejecución 

Este proyecto está pensado para ejecutar utilizando cabal

`cabal repl` : iniciar el interprete en el proyecto 

Una vez dentro del interprete se pueden llamar las funciones o crear los tableros, se brinda como parte del código una representación del tablero presente en la orientación del proyecto.

para resolverlo basta ejecutar:
`solved = solve sample`
`printBoard solved`

para resolver un tablero cualquiera (guardado en la variable board, sustituir por la variable en cuestión):
`solved = solve board`

para generar un tablero (filas, columnas, minNum, maxNum):
`board = generateBoard 5 5 1 25`

para imprimir el tablero resuelto:
`printBoard board`