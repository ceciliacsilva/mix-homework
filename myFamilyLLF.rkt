#lang racket
(require racklog)

(define %pai %empty-rel)
(define %mãe %empty-rel)
(define %masculino %empty-rel)
(define %feminino %empty-rel)
(define %casal %empty-rel)

(define %conjuge
  (lambda(A B)
    (%or (%casal A B)
         (%casal B A))))

(%assert! %pai ()
        [('jorge 'maria)]
        [('jorge 'mauro)]
        [('geraldo 'roberto)]
        [('geraldo 'carlos)]
        [('geraldo 'solange)])

(%assert! %mãe ()
          [('ana 'maria)]
          [('ana 'mauro)]
          [('helena 'roberto)]
          [('helena 'carlos)]
          [('helena 'solange)])

(%assert! %masculino ()
         [('jorge)]
         [('geraldo)]
         [('mauro)]
         [('roberto)]
         [('carlos)])

(%assert! %feminino ()
          [('ana)]
          [('helena)]
          [('maria)]
          [('solange)])

(define %genitor
  (lambda(A B)
    (%or (%mãe A B)
         (%pai A B))))

(define %irmão
  (lambda(A B)
    (%or (%and (%and (%masculino A)
                     (%and (%let (X) (%and (%genitor X A)
                                           (%genitor X B))
                                 
                                 )))
               (%/= A B)
               )
         #|(%and (%and (%masculino A)
                     (%and (%let (X) (%and (%genitor X A)
                                           (%genitor X B)))))
               ;(%not (equal? A B))
               (%/= A B)
               )|#
         )))
         
(define %irmã
  (lambda(A B)
    (%or (%and (%and (%feminino A)
                     (%and (%let (X) (%and (%genitor X A)
                                           (%genitor X B)))))
               (%/= A B)
               )
         #|(%and (%and (%feminino A)
                     (%and (%let (X) (%and (%genitor X A)
                                           (%genitor X B)))))
               ;(%not (equal? A B))
               (%/= A B)
               )|#
         )))

(define %avô
  (lambda(A B)
    (%or (%and (%masculino A)
               (%let (X Y)
                 (%and #|(%or|# (%genitor A X) #|(%and (%conjuge A Y) (%genitor Y X)))|#
                       (%genitor X B))))
         #|(%and (%masculino A)
               (%let (X)
                     (%and (%pai A X)
                           (%mãe X B))))|#)))

(define %avó
  (lambda(A B)
    (%or (%and (%feminino A)
               (%let (X Y)
                 (%and #|(%or|# (%genitor A X) #|(%and (%conjuge A Y) (%genitor Y X)))|#
                       (%genitor X B))))
         #|(%and (%feminino A)
               (%let (X)
                     (%and (%pai A X)
                           (%mãe X B))))|#)))

(define %tio
  (lambda(A B)
    (%or (%and (%masculino A)
               (%let (X Y)
                     (%and (%or (%irmão A X) (%and (%conjuge A Y) (%or (%irmão Y X) (%irmã Y X))))
                           (%genitor X B))))
         #|(%and (%masculino A)
               (%let (X)
                     (%and (%irmão X A)
                           (%pai X B))))|#
         )))
         
(define %tia
  (lambda(A B)
    (%or (%and (%feminino A)
               (%let (X Y)
                     (%and (%or (%irmã A X)(%and (%conjuge A Y) (%or (%irmão Y X) (%irmã Y X))))
                           (%genitor X B))))
         #|(%and (%feminino A)
               (%let (X)
                     (%and (%irmão X A)
                           (%pai X B))))|#
         )))



(%assert! %pai ()
          [('carlos 'andré)]
          [('geraldo 'lucas)]
          [('lucas 'pedro)]
          [('jorge 'joão)]
          [('joão 'yasmin)]
          [('marcos 'miguel)]
           )

(%assert! %mãe ()
          [('luana 'andré)]
          [('solange 'lúcia)]
          [('alice 'pedro)]
          [('eloísa 'lucas)]
          [('helena 'joão)]
          [('maria 'miguel)]
          )

(%assert! %feminino()
          [('luana)]
          [('alice)]
          [('eloísa)]
          [('lúcia)]
          [('yasmin)])

(%assert! %masculino()
         [('andré)]
         [('miguel)]
         [('marcos)]
         [('pedro)]
         [('lucas)]
         [('joão)])

(define irmão
  (lambda (A B)    
    (let [(lista '())]
    (let loop[(state (%which (X Y) (%and (%irmão X Y)
                                         (%and (%= A X)
                                               (%= B Y)))
                            ))]
      (cond [(null? lista)(set! lista (cons state lista))])
      (cond [state (set! state (%more))
                   ;(displayln state)
                   (cond[(not (pertence state lista))
                   (set! lista (cons state lista))])
                   (cond [state (set! state #t)])
                   (loop state)]
          ))
      ;(displayln (reverse lista))
      (let [(listaIrmao (reverse lista))]
        (for [(i (in-list listaIrmao))]
          (match i
            [(list x y)
             (displayln (~a (cdr x) " é irmão de " (cdr y)))]
            [_ ""])
            ))))
      )

(define irmã
  (lambda (A B)    
    (let [(lista '())]
    (let loop[(state (%which (X Y) (%and (%irmã X Y)
                                         (%and (%= A X)
                                               (%= B Y)))
                            ))]
      (cond [(null? lista)(set! lista (cons state lista))])
      (cond [state (set! state (%more))
                   ;(displayln state)
                   (cond[(not (pertence state lista))
                   (set! lista (cons state lista))])
                   (cond [state (set! state #t)])
                   (loop state)]
          ))
      ;(displayln (reverse lista))
      (let [(listaIrmao (reverse lista))]
        (for [(i (in-list listaIrmao))]
          (match i
            [(list x y)
             (displayln (~a (cdr x) " é irmã de " (cdr y)))]
            [_ ""])
            ))))
      )

(define tio
  (lambda (A B)    
    (let [(lista '())]
    (let loop[(state (%which (X Y) (%and (%tio X Y)
                                         (%and (%= A X)
                                               (%= B Y)))
                            ))]
      (cond [(null? lista)(set! lista (cons state lista))])
      (cond [state (set! state (%more))
                   ;(displayln state)
                   (cond[(not (pertence state lista))
                   (set! lista (cons state lista))])
                   (cond [state (set! state #t)])
                   (loop state)]
          ))
      ;(displayln (reverse lista))
      (let [(listaIrmao (reverse lista))]
        (for [(i (in-list listaIrmao))]
          (match i
            [(list x y)
             (displayln (~a (cdr x) " é tio de " (cdr y)))]
            [_ ""])
            ))))
      )

(define tia
  (lambda (A B)    
    (let [(lista '())]
    (let loop[(state (%which (X Y) (%and (%tia X Y)
                                         (%and (%= A X)
                                               (%= B Y)))
                            ))]
      (cond [(null? lista)(set! lista (cons state lista))])
      (cond [state (set! state (%more))
                   ;(displayln state)
                   (cond[(not (pertence state lista))
                   (set! lista (cons state lista))])
                   (cond [state (set! state #t)])
                   (loop state)]
          ))
      ;(displayln (reverse lista))
      (let [(listaIrmao (reverse lista))]
        (for [(i (in-list listaIrmao))]
          (match i
            [(list x y)
             (displayln (~a (cdr x) " é tia de " (cdr y)))]
            [_ ""])
            ))))
      )

(define avô
  (lambda (A B)    
    (let [(lista '())]
    (let loop[(state (%which (X Y) (%and (%avô X Y)
                                         (%and (%= A X)
                                               (%= B Y)))
                            ))]
      (cond [(null? lista)(set! lista (cons state lista))])
      (cond [state (set! state (%more))
                   ;(displayln state)
                   (cond[(not (pertence state lista))
                   (set! lista (cons state lista))])
                   (cond [state (set! state #t)])
                   (loop state)]
          ))
      ;(displayln (reverse lista))
      (let [(listaIrmao (reverse lista))]
        (for [(i (in-list listaIrmao))]
          (match i
            [(list x y)
             (displayln (~a (cdr x) " é avô de " (cdr y)))]
            [_ ""])
            ))))
      )

(define avó
  (lambda (A B)    
    (let [(lista '())]
    (let loop[(state (%which (X Y) (%and (%avó X Y)
                                         (%and (%= A X)
                                               (%= B Y)))
                            ))]
      (cond [(null? lista)(set! lista (cons state lista))])
      (cond [state (set! state (%more))
                   ;(displayln state)
                   (cond[(not (pertence state lista))
                   (set! lista (cons state lista))])
                   (cond [state (set! state #t)])
                   (loop state)]
          ))
      ;(displayln (reverse lista))
      (let [(listaIrmao (reverse lista))]
        (for [(i (in-list listaIrmao))]
          (match i
            [(list x y)
             (displayln (~a (cdr x) " é avó de " (cdr y)))]
            [_ ""])
            ))))
      )

(%assert! %casal ()
          [('helena 'geraldo)]
          [('alice 'lucas)]
          [('luana 'carlos)]
          [('maria 'marcos)]
          [('ana 'jorge)])





;;Funções extras
(define pertence (lambda (X L) 
                    (if (null? L) #f (if (equal? (car L) X) #t (pertence X (cdr L))))))
          