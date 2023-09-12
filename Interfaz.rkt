#lang racket/gui
(require wxme/image)
(require "grafo.rkt")


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

(define lista_posa null)
(define lista_posb null)


; Widgets del primer panel

(define grafoVacio ;Grafo que va a ser iterado y cambiado en cada iteración
  '())


(define listaA null)
(define listaB null)
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

                 
                 ;(print a)
                 ;(print lista_nodos)
                 
                 (set! grafoVacio (agregar_arista grafoVacio a b c))
                 (mostrar-grafo grafoVacio)

                 (define nodoA "")
                 (define nodoB "")
                 (define posx1 0)
                 (define posy1 0)
                 (define posx2 0)
                 (define posy2 0)
                 (cond ((miembro a lista_nodos))
                       (else
                        ;(dib_line (car lista_x) (car lista_y) (cadr lista_x) (cadr lista_y))
                        (define pos null)
                        (set! pos (append pos (list a (car lista_x) (car lista_y))))

                        (set! lista_posa (cdr (cons lista_posa pos)))
                        ;(print lista_pos)
                        ;NODO cadr lista_posa
                        ;X cadddr lista_posa
                        ;Y caddr lista_posa
                        
                        (set! nodoA(car lista_posa))
                        (set! posx1(cadr lista_posa))
                        (set! posy1(caddr lista_posa))
                        (set! listaA (append listaA lista_posa))
                        ;(print listaA)
                        ;(print nodoA)
                 
                        (dibujar (car lista_x) (car lista_y) a)
                        (set! lista_x (cdr lista_x))
                        (set! lista_y (cdr lista_y))
                        (set! lista_nodos(append lista_nodos (list a)))))
                 (cond ((miembro b lista_nodos) dib_line (car  lista_x) (car  lista_y) (cadr lista_x) (cadr lista_y))
                       (else
                        ;(dib_line (car lista_x) (car lista_y) (cadr lista_x) (cadr lista_y))
                        (define pos null)
                        (set! pos (append pos (list b (car lista_x) (car lista_y))))

                        (set! lista_posb (cdr (cons lista_posb pos)))
                        ;(print lista_pos)
                        ;NODO cadr lista_posb
                        ;X cadddr lista_posb
                        ;Y caddr lista_posb
                        
                        (set! nodoB(car lista_posb))
                        (set! posx2(cadr lista_posb))
                        (set! posy2(caddr lista_posb))
                        (set! listaB (append listaB lista_posb))
                        
                        (dibujar (car lista_x) (car lista_y) b)
                        (set! lista_x (cdr lista_x))
                        (set! lista_y (cdr lista_y))
                        (set! lista_nodos(append lista_nodos (list b)))))
                 (define listaT (append listaA listaB))
                 
                 ;(print (verificar a listaA))
                 ;(print (verificar b listaB))
                 (dib_line (car (verificar a listaT)) (cdr (verificar a listaT)) (car (verificar b listaT)) (cdr (verificar b listaT)))
                 )])

; Función para verificar si el nodo se creó anteriormente para generar la línea

(define (verificar nodoa lista1)
  (cond
    ((string=? nodoa (car lista1)) (cons (cadr lista1) (caddr lista1)))
    (else (verificar nodoa (cdddr lista1)))))


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
  (send dc2 set-brush "Pale Turquoise" 'solid)
  (send dc2 draw-ellipse x y 70 70)
  (send dc2 set-font (make-font #:size 10))
  (send dc2 draw-text nodo (+ x 5) (+ y 20)))


(define (dib_line x1 y1 x2 y2)
  (send dc2 set-brush "teal" 'solid)
  (send dc2 draw-spline (+ 35 x1) (+ 35 y1) (+ 35 x1) (+ 35 y1) (+ 35 x2) (+ 35 y2))
  (send dc2 draw-ellipse (+ 30 x2) (+ 30 y2) 10 10))



 
; Enseñar la ventana

(send ventana show #t)


