#lang racket

(define (grafoVacio)
  '())

(define (agregar-arista grafo origen destino peso)
  (cond
    ((equal? grafo '()) (list (list origen (list (list destino peso)))))
    (else 
      (cond ((nodo-existe? origen grafo)
        (define grafoMovido (mover-nodo-origen-al-inicio grafo origen))
        (define grafoXD  grafoMovido)
        (grafo_actualizado (list(list (caar grafoXD)(list (caadar grafoXD)(append (list destino peso))))) (cdr grafoMovido)))

        (else
          (append grafo (list (list origen (list (list destino peso))))))))))

(define (grafo_actualizado grafoNuevo grafoViejo)
  (cond ((null? grafoViejo) grafoNuevo)
    (else
      (cons (car grafoViejo) (grafo_actualizado grafoNuevo (cdr grafoViejo))))))

(define (mover-nodo-origen-al-inicio grafo nodo-origen)
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


(define (nodo-existe? nombre grafo)
  (define grafo2 grafo)
  (cond
    [(null? grafo) #f] ; Si el grafo está vacío, el nodo origen no existe.
    [(eq? nombre (caar grafo2)) #t] ; Compara el nombre con el nombre del primer nodo.
    [else
     (nodo-existe? nombre (cdr grafo2))]))

(define (mostrar-grafo grafo)
  (display "Grafo:")
  (displayln grafo))

(define (main)
  (define grafo1 (grafoVacio))
  (displayln "Grafo vacío creado.")
  
  (define grafo2 (agregar-arista grafo1 "Costa rica" 'B 1))
  (mostrar-grafo grafo2)

  (define grafo3 (agregar-arista grafo2 'B 'C 2))
  (mostrar-grafo grafo3)

  (define grafo4 (agregar-arista grafo3 'C 'D 3))
  (mostrar-grafo grafo4)

  (define grafo5 (agregar-arista grafo4 'D 'E 4))
  (mostrar-grafo grafo5)

  (define grafo6 (agregar-arista grafo5 'B 'J 5))
  (mostrar-grafo grafo6)

  (define grafo7 (agregar-arista grafo6 'C 'I 10))
  (mostrar-grafo grafo7)

  )


(main)
