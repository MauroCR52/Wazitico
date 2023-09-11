#lang racket/gui
(require wxme/image)

; Ventana principal
(define ventana (new frame% [label "Wazitico"]
                     [width 800]
                     [height 1000]
                     ))
 
(define canva1 (new canvas% 
     [parent ventana]
     [paint-callback
      (lambda (canvas dc)
        (send dc draw-bitmap (read-bitmap "WaziTico.png") -140 0)
        )]))

; Creación de los paneles a utilizar

(define panel1 (new vertical-panel% [parent ventana]
                                     [alignment '(center center)]
                                     ))

(define panel2 (new vertical-panel% [parent ventana]
                                     [alignment '(center top)]
                                     [min-height 400]))


; Listas con las posiciones donde van a aparecer los nodos

(define lista_x '(80 80 200 350 500 200 350 500 620 620 0))
(define lista_y  '(130 250 40 40 40 340 340 340 130 250 0))
(define lista_nodos null)


; Widgets del primer panel

(define lista "")
(define ele null)
(define texto1 (new text-field% [parent panel1] [label "            Ingrese un origen "]))
(define texto2 (new text-field% [parent panel1] [label "            Ingrese un destino"]))
(define texto3 (new slider%
                    (label "Ingrese una distancia ")
                    (parent panel1)
                    (min-value 1)
                    (max-value 25)
                    (init-value 1)))

; Este botón envía a crear el grafo al otro archivo para recibirlo
(new button% [parent panel1] [label "Aceptar"]
     [callback (lambda (button event)
                 (define a (send texto1 get-value))
                 (define b (send texto2 get-value))
                 (define c (send texto3 get-value))
                 (define d (list a b c))
                 ;(print lista_nodos)
                 ;(define g (list e f))
                 (cond
                   ((send choice1 find-string a) )
                   (else (send choice1 append a)))
                 (cond
                   ((send choice1 find-string b) )
                   (else (send choice1 append b)))
                 (cond
                   ((send choice2 find-string a) )
                   (else (send choice2 append a)))
                 (cond
                   ((send choice2 find-string b) )
                   (else (send choice2 append b)))

                 
                 (print d)
                 ;(print lista_nodos)

                 
                 (cond ((miembro a lista_nodos) print "HOLA"); dib_line (car  lista_x) (car  lista_y) (cadr lista_x) (cadr lista_y))
                       (else
                        (dib_line (car lista_x) (car lista_y) (cadr lista_x) (cadr lista_y))
                        (dibujar (car lista_x) (car lista_y) a)
                        (set! lista_x (cdr lista_x))
                        (set! lista_y (cdr lista_y))
                        (set! lista_nodos(append lista_nodos (list a)))))
                 (cond ((miembro b lista_nodos) dib_line (car  lista_x) (car  lista_y) (cadr lista_x) (cadr lista_y))
                       (else
                        ;(dib_line (car lista_x) (car lista_y) (cadr lista_x) (cadr lista_y))
                        (dibujar (car lista_x) (car lista_y) b)
                        (set! lista_x (cdr lista_x))
                        (set! lista_y (cdr lista_y))
                        (set! lista_nodos(append lista_nodos (list b)))))

                 )])

; Función miembro complementaria

(define (miembro ele lista)
  (cond((null? lista) #f)
       ((string=? ele (car lista)) #t)
       (else(miembro ele (cdr lista))))
  )

; Etiquetas para acomodar el espacio

(define acomodar0 (new message% [parent panel1] [label " "]))
(define acomodar1 (new message% [parent panel1] [label "Generador de rutas "]))


; Funciones para ingresar nodos, pesos y poder seleccionarlos para elegir rutas

(define choice1 (new choice%
                    (label "Seleccione el lugar de origen  ")
                    (parent panel1)
                    [min-width 300]
                    (choices null)))


(define acomodar2 (new message% [parent panel1] [label " "] ))


(define choice2 (new choice%
                    (label "Seleccione el lugar de destino  ")
                    (parent panel1)
                    [min-width 300]
                    (choices null)
                    ))


(define acomodar3 (new message% [parent panel1] [label " "]))


; Este botón es el que retorna la ruta más rápida y las demás


(new button% [parent panel1] [label "Generar ruta"]
     [callback (lambda (button event)
                 (define origen (send choice1 get-string-selection))
                 (define destino (send choice2 get-string-selection))
                 [print origen]
                 [print destino])])


; Se crea la canva donde se va a generar el grafo
(define canva2 (new canvas% 
                    [parent panel2]
                    )
)



; Funciones para dibujar nodos y aristas

(define dc2 (send canva2 get-dc))


(define (dibujar x y nodo)
  (send dc2 set-brush "green" 'solid)
  (send dc2 draw-ellipse x y 70 70)
  (send dc2 set-font (make-font #:size 10))
  (send dc2 draw-text nodo (+ x 5) (+ y 20)))


(define (dib_line x1 y1 x2 y2)
  (send dc2 set-brush "black" 'solid)
  (send dc2 draw-spline (+ 35 x1) (+ 35 y1) (+ 35 x1) (+ 35 y1) (+ 35 x2) (+ 35 y2)))



 
; Enseñar la ventana

(send ventana show #t)


