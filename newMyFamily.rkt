#lang racket
(require racklog)

(define %father    %empty-rel)
(define %mother    %empty-rel)
(define %male      %empty-rel)
(define %female    %empty-rel)
(define %couple    %empty-rel)

(define (%parent A B)
  (%or (%mother A B)
       (%father A B)) )

(define (%brother A B)
  (%and (%and (%male A)
              (%let (X)
                    (%and (%parent X A)
                          (%parent X B)) ))
        (%/= A B)) )

(define (%sister A B)
  (%and (%and (%female A)
              (%let (X)
                    (%and (%parent X A)
                          (%parent X B)) ))
        (%/= A B)) )

(define (%grandfather A B)
  (%and (%male A)
        (%let (X)
              (%and (%parent X B)
                    (%father A X)) )) )

(define (%grandmother A B)
  (%and (%female A)
        (%let (X)
              (%and (%parent X B)
                    (%mother A X)) )) )

(define (%uncle A B)
  (%let (X)
    (%and 
     (%brother A X)
     (%parent  X B)) ) )

(define (%aunt A B)
  (%let (X)
    (%and 
     (%sister A X)
     (%parent X B)) ) )

(define X (%let (X) X))
(define Y (%let (Y) Y))

(define (brother A B)
  (let loop ( (interation
               (%which (X Y)
                       (%and (%brother X Y)
                             (%and (%= A X)
                                   (%= B Y)) )))
              (result null) )
    (if interation
        (loop (%more) (cons interation result))
        (for ( (relation (reverse result)) )
          (match relation
            [(list x y)
             (displayln (~a (cdr x) " is " (cdr y) "'s brother")) ])
          )
        ))
    )

(define (sister A B)
  (let loop ( (interation
               (%which (X Y)
                       (%and (%sister X Y)
                             (%and (%= A X)
                                   (%= B Y)) )))
              (result null) )
    (if interation
        (loop (%more) (cons interation result))
        (for ( (relation (reverse result)) )
          (match relation
            [(list x y)
             (displayln (~a (cdr x) " is " (cdr y) "'s sister")) ])
          )
        ))
    )

(%assert! %father ()
          [('jorge 'maria)]
          [('jorge 'mauro)]
          [('geraldo 'roberto)]
          [('geraldo 'carlos)]
          [('geraldo 'solange)] )

(%assert! %mother ()
          [('ana 'maria)]
          [('ana 'mauro)]
          [('helena 'roberto)]
          [('helena 'carlos)]
          [('helena 'solange)])

(%assert! %male ()
          [('mauro)]
          [('jorge)]
          [('geraldo)]
          [('roberto)]
          [('carlos)] )

(%assert! %female ()
          [('maria)]
          [('solange)]
          [('helena)]
          [('ana)] )
