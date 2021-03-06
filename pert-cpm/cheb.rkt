#lang racket
(provide (all-defined-out))

;;File:       cheb.rkt
;;Author:     Cecília Carneiro e Silva
;;Descrition: Using chebyshev polynomials to calculate integrals and derivatives
;;Use:        >(define mycos (chebev (chebft 0 (/ pi 2) 16 cos)))
;;            > (mycos (/ pi 2))

;;            >(define icos (chebev (chint (chebft 0 (/ pi 2) 16 cos))))
;;            > (icos (/ pi 2))

;;            > (define dcos (chebev (chder (chebft 0 (/ pi 2) 16 cos))))
;;            > (dcos (/ pi 2))

;;            > (define j0 (BESSEL-J 0))
;;            > (j0 1.0)

;;            > (define xj0 (chebev (chebft 0.0 10.0 16 j0)))
;;            > (xj0 1.0)


(define (chebft a b n fn)
  (let [(f (make-vector n))
        (bma (* 0.5 (- b a)))
        (bpa (* 0.5 (+ b a)))
        (fac (/ 2.0 n))]
    (for [(k (in-range 0 n))]
      (let [(y (cos (* pi (/ (+ k 0.5) n))))]
        (vector-set! f k (apply fn (list (+ (* y bma) bpa))))))
    (let [(c (for/vector [(j (in-range n))]
              (* fac
                 (for/sum [(k (in-range n))]
                   (* (vector-ref f k) (cos (* pi j (/ (+ k 0.5) n)))))
                 )))]
      (list a b c)
    )))

(define (chebev abc)
  (lambda(x)
    (let* [(a (list-ref abc 0))
           (b (list-ref abc 1))
           (c (list-ref abc 2))
           (m (vector-length c))
           (y (/ (- (* 2.0 x) a b) (- b a)))
           (y2 (* 2.0 y))]
      (when (> (* (- x a) (- x b))  0.0)
        (error "Not in range"))
      (let* [(sv 0.0)
             (d 0.0)
             (dd sv)]
        (for [(j (in-range (- m 1) 0 -1))]
          (set! sv d)
          (set! d (+ (- (* y2 d) dd) (vector-ref c j)))
          (set! dd sv))
        (+ (- (* y d) dd) (* 0.5 (vector-ref c 0)))))))

(define (chint abc)
  (let* [(a (list-ref abc 0))
        (b (list-ref abc 1))
        (c (list-ref abc 2))
        (n (vector-length c))
        (cint (make-vector n))
        (con (* 0.25 (- b a)))
        (sum 0.0)
        (fac 1.0)]
    (for [(j (in-range 1 (- n 1)))]
      (vector-set! cint j (/ (* con (- (vector-ref c (- j 1)) (vector-ref c (+ j 1)))) j))
      (set! sum (+ sum (* fac (vector-ref cint j))))
      (set! fac (- fac)))
    (vector-set! cint (- n 1) (/ (* con (vector-ref c (- n 2))) (- n 1)))
    (set! sum (+ sum (* fac (vector-ref cint (- n 1)))))
    (vector-set! cint 0 (* 2.0 sum))
    (list a b cint)))
      
(define (chder abc)
  (let* [(a (list-ref abc 0))
         (b (list-ref abc 1))
         (c (list-ref abc 2))
         (n (vector-length c))
         (cder (make-vector n))
         (con (/ 2.0 (- b a)))]
    (vector-set! cder (- n 1) 0.0)
    (vector-set! cder (- n 2) (* 2.0 (- n 1) (vector-ref c (- n 1))))
    (for [(j (in-range (- n 3) -1 -1))]
      (vector-set! cder j (+ (vector-ref cder (+ j 2)) (* 2.0 (+ j 1)
                                                          (vector-ref c (+ j 1))))))
    (for [(j (in-range 0 n))]
      (vector-set! cder j (* (vector-ref cder j) con)))
    (list a b cder)
    ))

(define (poly-expand var coefs)
  (cond [(null? coefs) 0.0]
        [(null? (cdr coefs)) (car coefs)]
        [else (eval `(+ ,(car coefs) (* ,var
                                  ,(poly-expand var  (cdr coefs)))))]))

(define (BESSEL-J order)
  (lambda(x)
    (cond [(= order 0)
           (cond [(<= (abs x) 3)
                  (let [(z2 (expt (/ x 3.0) 2))]
                    (poly-expand z2 '(1.0000000
                                    -2.2499997
                                    1.2656208
                                    -0.3163866
                                    0.0444479
                                    -0.0039444
                                    0.0002100)))]
                 [(>= x 3.0)
                  (let* [(z (/ 3.0 x))
                         (f0 (poly-expand z '( 0.79788456
                                               -0.00000077
                                               -0.00552740
                                               -0.00009512
                                               0.00137237
                                               -0.00072805
                                               0.00014476)))
                         (theta0 (+ x (poly-expand z '(-0.78539816
                                                       -0.04166397
                                                       -0.00003954
                                                       0.00262537
                                                       -0.00054125
                                                       -0.00029333
                                                       0.00013558))))]
                    (/ (* f0 (cos theta0)) (sqrt x)))]
                 [else (BESSEL-J order (- x))])]
          [(= order 1)
           (cond [(< (abs x) 3.0)
                  (let [(z2 (expt (/ x 3.0) 2))]
                    (* (* x (poly-expand z2 '(0.50000000
                                              -0.56249985
                                              0.21093573
                                              -0.03954289
                                              0.00443319
                                              -0.00031761
                                              0.00001109)))))]
                 [(>= x 3.0)
                  (let* [(z (/ 3.0 x))
                         (f1 (poly-expand z '( 0.79788456
                                               0.00000156
                                               0.01659667
                                               0.00017105
                                               -0.00249511
                                               0.00113653
                                               -0.00020033)))
                         (theta1 (+ x 
                                    (poly-expand z '	-2.35619449
                                                 +0.12499612
                                                 +0.00005650
                                                 -0.00637879
                                                 +0.00074348
                                                 +0.00079824
                                                 -0.00029166)))]
                    (/ (* f1 (cos theta1)) (/ sqrt x)))]
                 [else (- (BESSEL-J order (- x)))])]
          [(< order 0)
           (set! order (- order))
           (cond [(odd? order)
                  (- (BESSEL-J order x))]
                 [else (BESSEL-J order x)])]
          [else (- (* (/ (exact->inexact (* 2 (- order 1))) x)
                      (BESSEL-J (- order 1) x))
                   (BESSEL-J (- order 2) x))])))
  
  
                                       
