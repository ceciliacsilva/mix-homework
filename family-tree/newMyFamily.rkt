#|
File:       newMyFamily.rkt
Author:     Cecília Carneiro e Silva
Descrition: Simple prolog like Family-tree
Use:        > (brother)
            > (brother #:a 'mauro)
            > (brother #:b 'carlos)
            > (brother #:a 'mauro #:b 'carlos)
-> brother, sister, grandfather, grandmother, uncle, aunt
|#

#lang racket

(require racklog)

(provide (all-defined-out))

(define %father    %empty-rel)
(define %mother    %empty-rel)
(define %male      %empty-rel)
(define %female    %empty-rel)
(define %couple    %empty-rel)

;;relations
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

(%assert! %father ()                                                                                                                                 
          [('carlos 'andré)]                                                                                                                      
          [('geraldo 'lucas)]                                                                                                                     
          [('lucas 'pedro)]                                                                                                                       
          [('jorge 'joão)]                                                                                                                        
          [('joão 'yasmin)]                                                                                                                       
          [('marcos 'miguel)]                                                                                                                     
           )                                                                                                                                      
                                                                                                                                                  
(%assert! %mother ()                                                                                                                                 
          [('luana 'andré)]                                                                                                                       
          [('solange 'lúcia)]                                                                                                                     
          [('alice 'pedro)]                                                                                                                       
          [('eloísa 'lucas)]                                                                                                                      
          [('helena 'joão)]                                                                                                                       
          [('maria 'miguel)]                                                                                                                      
          )    


;; functions
(define (brother #:a [a (%let(A) A)] #:b [b (%let (B) B)])
  (let loop ( (interation
               (%which (X Y)
                       (%and (%brother X Y)
                             (%and (%= a X)
                                   (%= b Y)) )))
              (result null) )
    (if interation
        (loop (%more) (cons-not-repeat interation result))
        (for ( (relation (reverse result)) )
          (match relation
            [(list x y)
             (displayln (~a (cdr x) " is " (cdr y) "'s brother")) ])
          )
        ))
    )

(define (sister #:a [a (%let(A) A)] #:b [b (%let (B) B)])
  (let loop ( (interation
               (%which (X Y)
                       (%and (%sister X Y)
                             (%and (%= a X)
                                   (%= b Y)) )))
              (result null) )
    (if interation
        (loop (%more) (cons-not-repeat interation result))
        (for ( (relation (reverse result)) )
          (match relation
            [(list x y)
             (displayln (~a (cdr x) " is " (cdr y) "'s sister")) ])
          )
        ))
    )

(define (grandfather #:a [a (%let(A) A)] #:b [b (%let (B) B)])
  (let loop ( (interation
               (%which (X Y)
                       (%and (%grandfather X Y)
                             (%and (%= a X)
                                   (%= b Y)) )))
              (result null) )
    (if interation
        (loop (%more) (cons-not-repeat interation result))
        (for ( (relation (reverse result)) )
          (match relation
            [(list x y)
             (displayln (~a (cdr x) " is " (cdr y) "'s grandfather")) ])
          )
        ))
    )

(define (grandmother #:a [a (%let(A) A)] #:b [b (%let (B) B)])
  (let loop ( (interation
               (%which (X Y)
                       (%and (%grandmother X Y)
                             (%and (%= a X)
                                   (%= b Y)) )))
              (result null) )
    (if interation
        (loop (%more) (cons-not-repeat interation result))
        (for ( (relation (reverse result)) )
          (match relation
            [(list x y)
             (displayln (~a (cdr x) " is " (cdr y) "'s grandmother")) ])
          )
        ))
    )

(define (uncle #:a [a (%let(A) A)] #:b [b (%let (B) B)])
  (let loop ( (interation
               (%which (X Y)
                       (%and (%uncle X Y)
                             (%and (%= a X)
                                   (%= b Y)) )))
              (result null) )
    (if interation
        (loop (%more) (cons-not-repeat interation result))
        (for ( (relation (reverse result)) )
          (match relation
            [(list x y)
             (displayln (~a (cdr x) " is " (cdr y) "'s uncle")) ])
          )
        ))
    )

(define (aunt #:a [a (%let(A) A)] #:b [b (%let (B) B)])
  (let loop ( (interation
               (%which (X Y)
                       (%and (%aunt X Y)
                             (%and (%= a X)
                                   (%= b Y)) )))
              (result null) )
    (if interation
        (loop (%more) (cons-not-repeat interation result))
        (for ( (relation (reverse result)) )
          (match relation
            [(list x y)
             (displayln (~a (cdr x) " is " (cdr y) "'s aunt")) ])
          )
        ))
    )

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

;;cons the element in list if it isn't there yet
(define (cons-not-repeat element l)
  (if (member element l) l
      (cons element l)) )