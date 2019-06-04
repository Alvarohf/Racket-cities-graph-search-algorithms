#lang racket

;-------------------------------- LEER GRAFO DEL FICHERO -----------------------------------------

;Crea cada linea del grafo
(define (crea-grafo line)
  ;La lista seran los datos separados de la linea
  (let ([lista (string-split line)])
    (cond
      ;Si la linea es vacia devuelve la linea vacia
      [(empty? lista)'()]
      ;Si no construye la lista
      [else (list (car lista) ( list (cadr lista) (string->number (caddr lista))))])
    )
)

;Lee el grafo linea a linea
(define (read-graph file)
  ;Definimos linea como leer linea del archivo
  (let ([line (read-line file 'any)])
    ;Comprueba que no es el final del fichero
    (if (eof-object? line)
        empty
        ;Construye el grafo con los datos
        (cons (crea-grafo line) (read-graph file))
    )
  )
)

;Adapta el formato leido al formato de trabajo
(define (reducir-formato grafo)
   (cond
     ;Si esta vacio no devuelve nada
    [(empty? grafo) grafo]
    ;Si existen duplicados los obtiene y los elimina para dar el formato deseado
    [(not (empty?(comprobar-duplicados (caar grafo)(cdr grafo)))) (cons (cons (caar grafo) (comprobar-duplicados (caar grafo) grafo)) (reducir-formato (eliminar-duplicados (caar grafo) grafo)))]
    ;Si no se sigue construyendo el grafo con el nodo dado
    [else (cons (car grafo) (reducir-formato (cdr grafo)))]
       )
  )

;Comprueba si hay nodos con la ciudad de origen en comun y los fusiona en una lista
(define (comprobar-duplicados nodo grafo)
  (cond
    ;Si esta vacio no devuelve nada
    [(empty? grafo) empty]
    ;Si el nodo coincide con lo que buscamos se anade
    [(equal? nodo (caar grafo)) (append (cdar grafo) (comprobar-duplicados nodo (cdr grafo)))]
    ;Si no se vuelve a ejecutar para encontrar otras coincidencias
    [else (comprobar-duplicados nodo (cdr grafo))]
    ))

;Elimina los nodos del grafo que tengan el mismo nodo inicial que se pasa por parametro
(define (eliminar-duplicados nodo grafo)
  (cond
    ;Si esta vacio devuelve la lista vacia
    [(empty? grafo) (list)]
    ;Si el nodo que estamos buscando se repite entonces se salta
    [(equal? nodo (caar grafo)) (eliminar-duplicados nodo (cdr grafo))]
    ;Si no es el nodo que se busca entonces anade al grafo el valor
    [else (cons (car grafo) (eliminar-duplicados nodo (cdr grafo)))]
    ))

;Definimos el grafo que se va a emplear leyendolo del archivo
(define grafo (reducir-formato (call-with-input-file "entrada.txt" read-graph)))

;Pedir datos al usuario de origen, destino y busqueda
(display "Introduzca el origen: ")
(define origen (read-line))
(display "Introduzca el destino: ")
(define destino (read-line))
(display "Introduzca el tipo de busqueda(anchura,optimal,profundidad,bidireccional): ")
(define busqueda (read-line))
(display "Introduzca si desea emplear cerrados, excepto bidireccional (si/no): ")
(define cerrado (read-line))

;Estructura del nodo compuesta por camino y kilometros
(define-struct nodo (camino kilometros))

;Estado inicial y final
(define nodo-inicial (make-nodo (list origen) 0))
(define ciudad-final destino)
;Se usa en la busqueda bidireccional porque se busca por ambos lugares
(define nodo-final (make-nodo (list destino) 0))

;-------------------------------- INSERTAR NODOS ORDENADOS -----------------------------------------

;Inserta ordenado un nodo en la lista
(define (inserta-ordenado nodo lista)
  (cond
    ;Si esta vacia inserta el nodo y devuelve la nueva lista
    [(empty? lista) (list nodo)]
    ;Si los kilometros son menores construye la lista añadiendo primero el nodo
    [(< (nodo-kilometros nodo) (nodo-kilometros (car lista))) (cons nodo lista)]
    ;Si no se añade la cabeza de la lista y se vuelve a llamar a la funcion con la cola
    [else (cons (car lista) (inserta-ordenado nodo (cdr lista)))])
)

;Inserta varios nodos ordenados un en la lista
(define (inserta-ordenados-nodos nodos lista)
  (cond
    ;Si no hay nodos que insertar devuelve la lista
    [(empty? nodos) lista]
    ;Si hay nodos que insertar llama a insertar ordenados pasandole un nodo y repite el proceso con el siguiente nodo
    [else (inserta-ordenados-nodos (cdr nodos)(inserta-ordenado (car nodos) lista))])
)

;-------------------------------- EXPANDIR EL NODO -----------------------------------------

;Expansion de los sucesores del nodo dado
;Obtiene una lista con los sucesores del nodo dado
(define (sucesores lista nodo)
  (cond
    ;Si es vacia la lista no devuelve nada
    [(empty? lista) empty]
    ;Si no es vacia hace una lista con el nodo sucesor y con el resultado de las posteriores llamadas recursivas
    [(cons (make-nodo (cons (caar lista) (nodo-camino nodo)) (+ (cadar lista)(nodo-kilometros nodo))) (sucesores (cdr lista) nodo) )])
)
;Recorre el grafo para buscar los sucesores del nodo
(define (expandir grafo nodo)
  (cond
    ;Si es vacio no devuelve nada
    [(empty? grafo) empty]
    ;Si encuentra el nodo entonces llama a sucesores para sacarlos 
    [(equal? (caar grafo) (car (nodo-camino nodo)))(sucesores (cdar grafo) nodo)]
    ;Si no se vuelve a llamar a la funcion con los grupos restantes
    [else  (expandir (cdr grafo) nodo)])
)

;-------------------------------- IMPRIMIR POR PANTALLA -----------------------------------------

; Imprime el nodo con sus ciudades y distancia
(define (imprimir-camino nodo)
  ;Muestra el camino desde el origen al destino
  (display (reverse (nodo-camino nodo)))
  ;Muestra los kilometros del camino
  (display (nodo-kilometros nodo))
)
; Imprime todos los caminos de la lista
(define (muestra-caminos lista)
  (cond
    ;Si no esta vacia muestra el camino y kilometros de cada camino de la lista llamandose de forma recursiva
    [(> (length lista) 0)(display (reverse (nodo-camino (car lista)))) (display (nodo-kilometros (car lista)))(newline)(muestra-caminos (cdr lista))]
  )
)

;-------------------------------- BUSQUEDAS DEL CAMINO CON CERRADOS-----------------------------------------

;Comprueba si el nodo esta en la lista de cerrados devolviendo true o false
(define (esta-nodo-en-cerrados nodo cerrados)
  (cond
    ;Si esta vacia devuelve false
    [(empty? cerrados) #f]
    ;Si encuentra el elemento devuelve true
    [(equal? (car cerrados) nodo) #t]
    ;Si no la sigue recorriendo hasta encontrar otro elemento o dejarla vacia
    [else (esta-nodo-en-cerrados nodo (cdr cerrados))])
  )

;Busqueda en anchura con cerrados
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

;Busqueda en profundidad con cerrados
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
;Busqueda optimal con cerrados
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
;-------------------------------- BUSQUEDAS DEL CAMINO SIN CERRADOS-----------------------------------------

;Busqueda en profundidad 
(define (busqueda-en-profundidad abiertos)
  ;Si no esta vacia abiertos continua
  (unless (empty? abiertos)
    ;Definimos actual como el primero de abiertos
    (let ([actual (car abiertos)])
      ;Mostramos los datos de actual y abiertos
      (display "\nActual:\n-------\n")
      (imprimir-camino actual)
      (newline)
      (display "\nAbiertos:\n---------\n")
      (muestra-caminos (cdr abiertos))
      (cond
        ;Si coincide con el destino entonces devolvemos el camino y finaliza la busqueda
        [(equal? ciudad-final (car (nodo-camino actual))) (newline)(display "Camino final: ")(imprimir-camino actual) actual]
         ;Si no vuelve a realizar la busqueda expandiendo el nodo actual
        [else (busqueda-en-profundidad
               ;Al ser en profundidad tienen prioridad los hijos del nodo actual en abiertos (LIFO)
                   (append (expandir grafo actual) (cdr abiertos)))])
    )
   )
 )

;Busqueda optimal
(define (busqueda-optimal abiertos)
  ;Si no esta vacia abiertos continua
  (unless (empty? abiertos)
    ;Definimos actual como el primero de abiertos
    (let ([actual (car abiertos)])
      ;Mostramos los datos de actual y abiertos
      (display "\nActual:\n-------\n")
      (imprimir-camino actual)
      (newline)
      (display "\nAbiertos:\n---------\n")
      (muestra-caminos (cdr abiertos))
      (cond
        ;Si coincide con el destino entonces devolvemos el camino y finaliza la busqueda
        [(equal? ciudad-final (car (nodo-camino actual)))(display "Camino final: ")(imprimir-camino actual) actual]
        ;Si no vuelve a realizar la busqueda expandiendo el nodo actual
        [else (busqueda-optimal
               ;Al ser optimal tienen prioridad los que tienen menor coste de camino
                   (inserta-ordenados-nodos (expandir grafo actual) (cdr abiertos)))])
    )
   )
 )

;Busqueda en anchura 
(define (busqueda-en-anchura abiertos)
  ;Si no esta vacia abiertos continua
  (unless (empty? abiertos)
    ;Definimos actual como el primero de abiertos
    (let ([actual (car abiertos)])
      ;Mostramos los datos de actual y abiertos
      (display "\nActual:\n-------\n")
      (imprimir-camino actual)
      (newline)
      (display "\nAbiertos:\n---------\n")
      (muestra-caminos (cdr abiertos))
      (cond
        ;Si coincide con el destino entonces devolvemos el camino y finaliza la busqueda
        [(equal? ciudad-final (car (nodo-camino actual)))(display "Camino final: ")(imprimir-camino actual) actual]
        ;Si no vuelve a realizar la busqueda expandiendo el nodo actual
        [else (busqueda-en-anchura
               ;Al ser en anchura tienen prioridad los abiertos mas antiguos (FIFO)
                   (append (cdr abiertos) (expandir grafo actual)))])
    )
   )
 )

;Busqueda bidireccional
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

;-------------------------------- COMPROBAR COINCIDENCIAS ENTRE LISTAS DE ABIERTOS(Aux de busq. bidireccional) -----------------------------------------

;Para obtener el nodo en comun entre dos listas de abiertos iremos comparandolos
;Comprueba los distintos caminos del origen con todos los caminos del destino
(define (comprobar-caminos-en-caminos caminos-origen caminos-destino)
  (cond
    ;Si los caminos de origen son vacios devuelve la lista vacia
    [(empty? caminos-origen)(list)]
    ;Si los caminos de destino son vacios devuelve la lista vacia
    [(empty? caminos-destino) (list)]
    ;Si no toma el primer camino de los caminos del origen junto a todos los del destino mas el coste de este primer camino despues lo hara sucesivamente con el resto
    [else (append (comprobar-camino-en-caminos caminos-destino (nodo-camino (car caminos-origen)) (nodo-kilometros (car caminos-origen)))
                  (comprobar-caminos-en-caminos (cdr caminos-origen) caminos-destino))])
  )
;Comprueba el camino de origen con todos los caminos que tiene de destino
(define (comprobar-camino-en-caminos caminos-destino camino-origen coste-origen)
  (cond
     ;Si los caminos de origen son vacios devuelve la lista vacia
    [(empty? camino-origen) (list)]
    ;Si no toma el primer nodo del camino del origen junto a todos los del destino mas el coste de este primer camino y el camino, despues lo hara sucesivamente con el resto de nodos
    [else (append (comprobar-nodo-varios-caminos caminos-destino (car camino-origen) camino-origen coste-origen)
                  (comprobar-camino-en-caminos caminos-destino (cdr camino-origen)  coste-origen))])
  )
;Comprueba los nodos del camino de origen con todos los caminos del destino
(define (comprobar-nodo-varios-caminos caminos-destino nodo-origen camino-origen coste-origen)
  (cond
    ;Si los caminos de destino son vacios devuelve la lista vacia
    [(empty? caminos-destino) (list)]
    ;Si no se trata de comprobar si el camino del destino con su distancia es igual al nodo origen original, despues se repite con el resto de caminos de destino
    [else (append (comprobar-nodo-camino (nodo-camino (car caminos-destino)) (nodo-kilometros (car caminos-destino)) camino-origen nodo-origen coste-origen)
                  (comprobar-nodo-varios-caminos (cdr caminos-destino) nodo-origen camino-origen coste-origen))])
  )
;Comprueba los nodos del camino de origen con un camino del destino
(define (comprobar-nodo-camino camino-destino coste-destino camino-origen  nodo-origen coste-origen)
  (cond
    ;Si el camino de destino esta vacio devuelve la lista vacia
    [(empty? camino-destino) (list)]
    ;Si coincide el nodo con el camino significa que hay un nodo en comun por lo que se devuelve el nodo creado con el camino y coste de distancia
    [(equal? (car camino-destino) nodo-origen) (list (make-nodo (append (reverse (cdr camino-destino)) camino-origen ) (+ coste-origen coste-destino)))]
    ;Si no sigue comprobando el nodo con el resto de nodos del camino
    [else (comprobar-nodo-camino (cdr camino-destino) coste-destino camino-origen nodo-origen coste-origen)])
  )

;-------------------------------- ELEGIR LA BUSQUEDA -----------------------------------------

;Se permite elegir la busqueda
(define (elegir-busqueda busqueda cerrado)
(cond
  ;Comprobamos si se quiere con cerrados o no
  [(equal? cerrado "no")
   (cond
     ;Comprobamos el tipo de busqueda que se quiere
     ;Se hara la busqueda por el metodo y con el camino que se devuelva se obtendra el graphViz correspondiente con el camino resaltado
        [(equal? "optimal" busqueda)(busqueda-optimal (list nodo-inicial))]
        [(equal? "anchura" busqueda)(busqueda-en-anchura (list nodo-inicial))]
        [(equal? "profundidad" busqueda)(display "CUIDADO pueden producirse bucles infinitos")(busqueda-en-profundidad (list nodo-inicial))]
        [(equal? "bidireccional" busqueda)(busqueda-bidireccional(list nodo-inicial) (list nodo-final))]
        [else "ERROR: No se puede generar porque la busqueda no es correcta"]
  )
   ]
  [(equal? cerrado "si") (cond
     ;Comprobamos el tipo de busqueda que se quiere
     ;Se hara la busqueda por el metodo y con el camino que se devuelva se obtendra el graphViz correspondiente con el camino resaltado
      [(equal? "optimal" busqueda)(busqueda-optimal-cerrados (list nodo-inicial) (list) (length grafo))]
      [(equal? "anchura" busqueda)(busqueda-en-anchura-cerrados (list nodo-inicial) (list) (length grafo))]
      [(equal? "profundidad" busqueda)(busqueda-en-profundidad-cerrados (list nodo-inicial) (list) (length grafo))]
      [(equal? "bidireccional" busqueda)(busqueda-bidireccional(list nodo-inicial) (list nodo-final))]
      [else "ERROR: No se puede generar porque la busqueda no es correcta"]
  )]
  [else "ERROR: No se puede generar porque no indico correctamente si queria usar cerrados"])
)
(elegir-busqueda busqueda cerrado)