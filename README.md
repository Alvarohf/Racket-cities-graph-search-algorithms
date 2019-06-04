# Racket-cities-graph-search-algorithms
Algorithms to search for the shortest path in a graph of cities in the language Racket. (Uniform cost search, depth-first search, breadth first search and bidirectional search) 
## Algorithms
All the algorithms have closed-list, except bidirectional search.
### Uniform cost search
```racket
(define (busqueda-optimal-cerrados abiertos cerrados tam-grafo)
  ;Si no esta vacia abiertos continua
  (unless (empty? abiertos)
    ;Definimos actual como el primero de abiertos
    (let ([actual (car abiertos)])
      ;Comprobamos que el nodo actual no haya sido visitado ya en cerrados
      (cond
        ;Si ya ha sido visitados se vuelve a buscar con el siguiente nodo de abiertos
        [(esta-nodo-en-cerrados (car (nodo-camino actual)) cerrados) (busqueda-optimal-cerrados (cdr abiertos) cerrados tam-grafo)]
        ;Si no se comprueba si es el final
        ;Mostramos los datos de actual y abiertos
        [else (display "\nActual:\n-------\n")
         (imprimir-camino actual)
         (newline)
         (display "\nAbiertos:\n---------\n")
         (muestra-caminos (cdr abiertos))
         (cond
           ;Si coincide con el destino entonces devolvemos el camino y finaliza la busqueda
           [(equal? ciudad-final (car (nodo-camino actual))) (newline)(display "Camino final: ")(imprimir-camino actual) actual]
            ;Si la lista de abiertos excede el tamano del grafo entonces finaliza la ejecucion y devuelve nodo de fallo
           [(>= (length cerrados) (- tam-grafo 1)) (display "\nSe han recorrido todos los nodos pero no se ha encontrado el camino\n") (make-nodo (list) -1)]
           ;Si no vuelve a realizar la busqueda expandiendo el nodo actual
           [else (busqueda-optimal-cerrados
                  ;Al ser optimal tienen prioridad los que tienen menor coste de camino
                   (inserta-ordenados-nodos (expandir grafo actual) (cdr abiertos)) (cons (car (nodo-camino actual)) cerrados) tam-grafo)])])
      
    )
   )
 )
```
### Depth-first search
```racket
(define (busqueda-en-profundidad-cerrados abiertos cerrados tam-grafo)
   ;Si no esta vacia abiertos continua
  (unless (empty? abiertos)
     ;Definimos actual como el primero de abiertos
    (let ([actual (car abiertos)])
      ;Comprobamos que el nodo actual no haya sido visitado ya en cerrados
      (cond
         ;Si ya ha sido visitados se vuelve a buscar con el siguiente nodo de abiertos
        [(esta-nodo-en-cerrados (car (nodo-camino actual)) cerrados) (busqueda-en-profundidad-cerrados (cdr abiertos) cerrados tam-grafo)]
        ;Si no se comprueba si es el final
        ;Mostramos los datos de actual y abiertos
        [else (display "\nActual:\n-------\n")
         (imprimir-camino actual)
         (newline)
         (display "\nAbiertos:\n---------\n")
         (muestra-caminos (cdr abiertos))
         (cond
            ;Si coincide con el destino entonces devolvemos el camino y finaliza la busqueda
           [(equal? ciudad-final (car (nodo-camino actual))) (newline)(display "Camino final: ")(imprimir-camino actual) actual]
           ;Si la lista de abiertos excede el tamano del grafo entonces finaliza la ejecucion y devuelve nodo de fallo
           [(>= (length cerrados) (- tam-grafo 1)) (display "\nSe han recorrido todos los nodos pero no se ha encontrado el camino\n") (make-nodo (list) -1)]
            ;Si no vuelve a realizar la busqueda expandiendo el nodo actual
           [else (busqueda-en-profundidad-cerrados
                  ;Al ser en profundidad tienen prioridad los hijos del nodo actual en abiertos (LIFO)
                   (append (expandir grafo actual) (cdr abiertos)) (cons (car (nodo-camino actual)) cerrados) tam-grafo)])])
    )
   )
 )
```
### Breadth first search
```racket
(define (busqueda-en-anchura-cerrados abiertos cerrados tam-grafo)
  ;Si no esta vacia abiertos continua
  (unless (empty? abiertos)
    ;Definimos actual como el primero de abiertos
    (let ([actual (car abiertos)])
      ;Comprobamos que el nodo actual no haya sido visitado ya en cerrados
      (cond
        ;Si ya ha sido visitados se vuelve a buscar con el siguiente nodo de abiertos
        [(esta-nodo-en-cerrados (car (nodo-camino actual)) cerrados) (busqueda-en-anchura-cerrados (cdr abiertos) cerrados tam-grafo)]
        ;Si no se comprueba si es el final
        ;Mostramos los datos de actual y abiertos
        [else (display "\nActual:\n-------\n")
         (imprimir-camino actual)
         (newline)
         (display "\nAbiertos:\n---------\n")
         (muestra-caminos (cdr abiertos))
         (cond
           ;Si coincide con el destino entonces devolvemos el camino y finaliza la busqueda
           [(equal? ciudad-final (car (nodo-camino actual))) (newline)(display "Camino final: ")(imprimir-camino actual) actual]
           ;Si la lista de abiertos excede el tamano del grafo entonces finaliza la ejecucion y devuelve nodo de fallo
           [(>= (length cerrados) (- tam-grafo 1)) (display "\nSe han recorrido todos los nodos pero no se ha encontrado el camino\n") (make-nodo (list) -1)]
           ;Si no vuelve a realizar la busqueda expandiendo el nodo actual
           [else (busqueda-en-anchura-cerrados
                  ;Al ser en anchura tienen prioridad los abiertos mas antiguos (FIFO)
                   (append (cdr abiertos) (expandir grafo actual)) (cons (car (nodo-camino actual)) cerrados) tam-grafo)])])
    )
   )
 )
```
### Bidirectional search
```racket
(define (busqueda-bidireccional abiertos-origen abiertos-destino)
      ;Solo usaremos la rama cuando no este vacia
    (unless (empty? abiertos-origen)
    ;definimos el nodo actual como el primero de abiertos
    (let ([actual-origen (car abiertos-origen)])
      ;Mostraos el actual y abiertos
      (display "\nActual desde origen:\n-------\n")
      (imprimir-camino actual-origen)
      (newline)
      (display "\nAbiertos desde origen:\n---------\n")
      (muestra-caminos (cdr abiertos-origen))   
      (unless (empty? abiertos-destino)
          ;definimos el nodo actual como el primero de abiertos
          (let ([actual-destino (car abiertos-destino)])
          ;Mostraos el actual y abiertos
          (display "\nActual desde destino:\n-------\n")
          (imprimir-camino actual-destino)
          (newline)
          (display "\nAbiertos desde destino:\n---------\n")
          (muestra-caminos (cdr abiertos-destino))
          ;Comprobamos si es el nodo final o expandimos con un nuevo nodo
          (cond
            [(not (empty? (comprobar-caminos-en-caminos abiertos-origen abiertos-destino))) (display "Camino final: ") (imprimir-camino (car (comprobar-caminos-en-caminos abiertos-origen abiertos-destino))) (car (comprobar-caminos-en-caminos abiertos-origen abiertos-destino))]
            [else (busqueda-bidireccional (inserta-ordenados-nodos (expandir grafo actual-origen) (cdr abiertos-origen))
                                          (inserta-ordenados-nodos (expandir grafo actual-destino) (cdr abiertos-destino)))])
          )
       )
    )
    ) 
)
```
## Visualization in GraphViz
You can visualize the results of the search in a graph with graphViz. It will generate the full graph in colors but it highlights the ones you must follow to reach your destination from origin.
![Image of graph](https://github.com/Alvarohf/Racket-cities-graph-search-algorithms/blob/master/grafo.png)
