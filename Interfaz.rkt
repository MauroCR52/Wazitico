#lang racket/gui
(require wxme/image)

; Creamos la ventana principal
(define ventana (new frame% [label "Wazitico"]
                     [width 800]
                     [height 800]))
 
(new canvas% 
     [parent ventana]
     [paint-callback
      (lambda (canvas dc)
        (send dc draw-bitmap (read-bitmap "WaziTico.png") -140 0))])

; Paneles

(define panel1 (new vertical-panel% [parent ventana]
                                     [alignment '(center top)]))

(define panel2 (new vertical-panel% [parent ventana]
                                     [alignment '(center top)]))


; Widgets primer panel
(define lista "")
(define ele null)
(define texto1 (new text-field% [parent panel1] [label "            Ingrese un origen "]))
(new text-field% [parent panel1] [label "            Ingrese un destino"])
(new text-field% [parent panel1] [label "    Ingrese una distancia (1-10 Km)"])
(new button% [parent panel1] [label "Aceptar"]
     [callback (lambda (button event)
                 (define a (list lista))
                 (set! lista (append a (list (send texto1 get-value))))
                 (print lista))])

(define acomodar1 (new message% [parent panel1] [label "\n "]))

(define choice (new choice%
                    (label "Seleccione el lugar de origen  ")
                    (parent panel1)
                    (choices (list lista))))

(define acomodar2 (new message% [parent panel1] [label " "]))

(define choice2 (new choice%
                    (label "Seleccione el lugar de destino  ")
                    (parent panel1)
                    (choices (list "A" "B" "C" "D"))))

(define acomodar3 (new message% [parent panel1] [label " "]))

(new button% [parent panel1] [label "Generar mapa"])


; Widgets segundo panel




; Widgets tercer panel





 
; Show the dialog
(send ventana show #t)