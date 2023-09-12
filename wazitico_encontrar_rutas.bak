#lang racket
(define g '((C((E 4)))(B((C 2)))(A((B 2)(E 6)))))


;invertir un nodo y sus elementos
(define (invertir invertirAux rutas)
  (cond ( (null? rutas) invertirAux)
        (else
         (invertir (append (list (reverse (car rutas))) invertirAux)(cdr rutas)))))

;Indicar los nodos adyacentes de un nodo
(define (adyacente nodo grafo)
  (adyacente_aux (assoc nodo grafo) nodo grafo))  ;Assoc retorna la lista que cumpla que el car, es igual al elemento dado

(define (adyacente_aux comprobar nodo grafo) ;
  (cond((equal? comprobar  #f)#f)  
       (else(cond ((null? (cdr comprobar))
               cdr comprobar)
              (else (cadr comprobar))))))

;comprueba si un nodo está en la lista de destinos de un nodo. False si no, True si sí
(define (comprobar_nodo_ruta nodo lista)
  (cond ((null? lista) #f)
        ((equal? nodo (car lista))#t)
        (else
         (comprobar_nodo_ruta nodo (cdr lista)))))

;Retorna las relaciones de los nodos con la forma (destino origen), para ayudar a la función de ecnontrar todas las rutas posibles
(define (nodos_vecinos ruta grafo)
  (nodos_vecinos_aux ruta '() grafo (adyacente (car ruta) grafo))
  )
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


;Comprueba si en un recorrido, se llegó del nodo origen, al destino
(define (comprobar_destino destino recorrido)
  (equal? destino (car recorrido)))

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
(displayln (todas_rutas 'A 'B '((C((E 4)))(B ((C 2)))(A ((B 2)(E 6))) (E((A 1))))))


;(assoc 'B(cdr(assoc 'A g)))

;retorna una lista con el peso de todas las rutas, utilizando la lista obtenida en todas_rutas

;(define (peso_todas_rutas)
;  (cond ((null? ruta) num)
;        (else
;         (distanciaTotalRuta(+ num (car))))))







