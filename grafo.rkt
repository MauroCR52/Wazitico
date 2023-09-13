#lang racket

(provide agregar_arista mostrar-grafo golazo)



;Funcion que agrega un nodo junto con el destino y el peso al grafo
(define (agregar_arista grafo origen destino peso)
  (cond
    ((equal? grafo '()) (list (list origen (list (list destino peso)))))
    (else 
      (cond ((nodo_existe? origen grafo) ;en caso de que ya haya un nodo con el origen que vamos a meter
        ;Movemos el nodo con el destino existente al inicio del grafo para manipular mejor el grafo
        (define grafo_movido (mover_al_inicio grafo origen))

        ;Aquí vamos a separar el grafo en dos diferentes, una compuesta solo con el nodo con el origen a insertar
        ;y la otra con el resto del grafo para despues ir insertando uno por uno los nodos de este la nuevo grafo actualizado
        (define grafo_a_actualizar  grafo_movido)

        (grafo_actualizado (list(list (caar grafo_a_actualizar)(append (cadar grafo_a_actualizar)(list(list destino peso))))) (cdr grafo_movido)))
        
        (else
          (append grafo (list (list origen (list (list destino peso))))))))))

;Funcion que une el grafo con el nuevo destino al grafo con el resto de nodos
(define (grafo_actualizado grafo_nuevo grafo_sin_origen)
  (cond ((null? grafo_sin_origen) grafo_nuevo)
    (else
      (cons (car grafo_sin_origen) (grafo_actualizado grafo_nuevo (cdr grafo_sin_origen))))))

;Funcion que busca un nodo de origen del grafo y lo pasa al inicio
(define (mover_al_inicio grafo nodo-origen)
  (define (auxiliar grafo nodo-origen coincidentes no-coincidentes)
    (cond
      ((null? grafo)
       (append coincidentes (reverse no-coincidentes)))
      ((equal? (caar grafo) nodo-origen)
       (auxiliar (cdr grafo) nodo-origen (append coincidentes (list (car grafo))) no-coincidentes))
      (else
       (auxiliar (cdr grafo) nodo-origen coincidentes (append no-coincidentes (list (car grafo)))))))
  (auxiliar grafo nodo-origen '() '()))

(define (eliminar-nodo-origen grafo nodo-origen)
  (cond
    ((null? grafo) '())                       ; Si la lista es vacía, retornar lista vacía.
    ((equal? (caar grafo) nodo-origen)         ; Si el nodo origen coincide con el nodo actual.
     (eliminar-nodo-origen (cdr grafo) nodo-origen)) ; Llamar recursivamente sin incluir el nodo actual.
    (else (cons (car grafo)                    ; Si no coincide, mantener el nodo actual
                (eliminar-nodo-origen (cdr grafo) nodo-origen))))) ; Llamar recursivamente para el resto de la lista.

;Funcion booleana que recibe un nodo e indica si ya hay un nodo de origen con el nodo indicado
(define (nodo_existe? nombre grafo)
  (define grafo2 grafo)
  (cond
    ((null? grafo) #f)
    ((string=? nombre (caar grafo))  #t) 
    (else
     (nodo_existe? nombre (cdr grafo)))))

(define (mostrar-grafo grafo)
  (display "Grafo:")
  (displayln grafo))

(define (main)
  (define grafo1 '())
  (displayln "Grafo vacío creado.")
  
  (define grafo2 (agregar_arista grafo1 "A" "B" 2))
  (mostrar-grafo grafo2)

  (define grafo3 (agregar_arista grafo2 "B" "C" 2))
  (mostrar-grafo grafo3)

  (define grafo4 (agregar_arista grafo3 "C" "E" 4))
  (mostrar-grafo grafo4)

  (define grafo5 (agregar_arista grafo4 "B" "E" 6))
  (mostrar-grafo grafo5)
  
  )


;FUNCIONES PARA ENCONTRAR RUTAS


;invertir un nodo y sus elementos
;Entra: nodo y listas
;Sale: lista de listas
(define (invertir invertirAux rutas)
  (cond ((null? rutas) invertirAux)
        (else
         (invertir (append (list (reverse (car rutas))) invertirAux)(cdr rutas)))))

;Entra: un nodo y un grafo
;Sale: lista con nodos adyacentes
;Indicar los nodos adyacentes de un nodo
(define (adyacente nodo grafo)
  (adyacente_aux (assoc nodo grafo) nodo grafo))  ;Assoc retorna la lista que cumpla que el car, es igual al elemento dado
(define (adyacente_aux comprobar nodo grafo) ;
  (cond((equal? comprobar  #f)#f)  
       (else(cond ((null? (cdr comprobar))
               cdr comprobar)
              (else (cadr comprobar))))))

;comprueba si un nodo está en la lista de destinos de un nodo. False si no, True si sí
;Entra: nodo y una lista 
;Sale: true o false
(define (comprobar_nodo_ruta nodo lista)
  (cond ((null? lista) #f)
        ((equal? nodo (car lista))#t)
        (else
         (comprobar_nodo_ruta nodo (cdr lista)))))
;Entra: una ruta, y un grafi 
;Sale: una lista
;Retorna las relaciones de los nodos con la forma (destino origen), para ayudar a la función de ecnontrar todas las rutas posibles
(define (nodos_vecinos ruta grafo)
  (nodos_vecinos_aux ruta '() grafo (adyacente (car ruta) grafo)))
(define (nodos_vecinos_aux ruta rutaGenerada grafo adyacente)
  (cond ((null? adyacente) rutaGenerada)
        (else
         (cond ((comprobar_nodo_ruta (caar adyacente) ruta)
                (nodos_vecinos_aux ruta rutaGenerada grafo (cdr adyacente)))
               (else
                 (nodos_vecinos_aux ruta(append (list (cons (caar adyacente) ruta)) rutaGenerada) grafo (cdr adyacente))))
       )
     )
  )
;Entra:  un nodo y una lista
;Sale: true o false
;Comprueba si en un recorrido, se llegó del nodo origen, al destino
(define (comprobar_destino destino recorrido)
  (equal? destino (car recorrido)))

;Entra: Nodo origen nodo destino y grafo 
;Sale: lista de listas
;retornar todas las rutas posibles de un punto a, hacia un punto b
(define (todas_rutas origen destino g)
  (todas_rutas_aux (list (list origen)) destino g '()))
(define (todas_rutas_aux rutas destino grafo final)
  (cond ((null? rutas)
         (invertir '() final))
        ((comprobar_destino destino (car rutas))
         (todas_rutas_aux (cdr rutas) destino grafo (cons (car rutas) final) ))
        (else
         (todas_rutas_aux
          (append (nodos_vecinos (car rutas) grafo) (cdr rutas)) destino grafo final))))

;Entra: Nodo origen nodo destino y grafo
;Sale: número entero
;retorna el peso de una relación origen destino
(define (obtener_peso_O_D origen destino grafo)
  (car(cdr(assoc destino(cadr(assoc origen grafo))))))

;Entra: una lista y grafo
;Sale: número entero
;agarra una sublista con una ruta, y me retorna su peso total.
(define (peso_total_ruta ruta grafo)
  (peso_total_ruta_aux ruta 0 grafo))
(define (peso_total_ruta_aux lista suma grafo)
  (cond((empty? (cdr lista))suma)  
       (else (peso_total_ruta_aux(cdr lista)(+ suma(obtener_peso_O_D (car lista) (cadr lista) grafo)) grafo))))

;Entra: lista de listas y grafo
;Sale: listas de listas
;función que coloca enfrente de todas las listas su peso (hace un centro a la función rutas_ordenadas para que meta gol)
(define (centro todas_rutas grafo)
  (centro_aux todas_rutas '() grafo))
(define (centro_aux lista lista_n grafo)
    (cond((empty?  lista)lista_n)
         (else (centro_aux (cdr lista)(cons(cons(peso_total_ruta(car lista)grafo)(car lista)) lista_n)grafo))))
;Entra: Una lista de listas
;Sale: Una lista de listas ordenada
;ORDENA todas las rutas dependiendo de su primer nodo
(define (comparar-sublistas a b)
  (< (car a) (car b)))
(define (lista-ordenada lst)
  (sort lst comparar-sublistas))
;Entra: Nodo origen nodo destino y grafo
;Sale: Listas de listas con peso ordenadas.
;después de recibir el centro de la función centro, se entrega al usuario las rutas
(define (golazo origen destino g)
  (lista-ordenada (sort(centro(todas_rutas origen destino g) g) comparar-sublistas)))



;(main)
