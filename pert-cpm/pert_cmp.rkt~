#lang racket

#|Implementação PERT-CMP, para gerenciamento de projetos.
Cecília Carneiro e Silva.

Para usar: 
 (geraJS 'A 'O var G) ;;Gera os resultados, para o grafo G, atividade início A e final O

Arquivo de saída cpmOut.html
|#
(include "cheb.rkt")
(require plot)
(plot-new-window? #t)

(define (makeTempos tmp)
  (lambda(x)
    (cond [(assoc x tmp)
           (cadr (assoc x tmp))])))

(define (makePiorTempos tmp)
  (lambda(x)
    (cond [(assoc x tmp)
           (list-ref (assoc x tmp) 3)])))

(define (makeVariancia tmp)
  (lambda(x)
    (cond [(assoc x tmp)
           (list-ref (assoc x tmp) 2)])))

(define var '(A B C D E F G H I J K L M N O))

(define gEstimativas '((A 1 2 3) (B 2 3.5 8) (C 6 9 18) (D 4 5.5 10)
                                 (E 1 4.5 5) (F 4 4 10) (G 5 6.5 11)
                                 (H 5 8 17) (I 3 7.5 9) (J 3 9 9) (K 4 4 4)
                                 (L 1 5.5 7) (M 1 2 3) (N 5 5.5 9) (O 0 0 0)))

(define dur2 (makePiorTempos gEstimativas))

(define gMedioDesvio '())

(define dur
  (let [(a 
         (for/list [(i (in-list gEstimativas))]
           (match i
             [(list e o m p) (list e (/ (+ o (* 4 m) p) 6) (sqr (/ (- p o) 6)))])))]
    ;(displayln a)
    (set! gMedioDesvio a)
    (makeTempos a)))

(define durVariancia (makeVariancia gMedioDesvio))

#|(define dur (makeTempos '((A 2) (B 4) (C 10) (D 6) (E 4) (F 5) (G 7) 
                                (H 9) (I 7) (J 8) (K 4) (L 5) (M 2) (N 6) (O 0))))|#

(define G '((A B) (B C) (C D) (C E) (C I) (D G) (G H) (H M) (E H) 
                  (E F) (F J)(J K) (K N) (J L) (L N) (I J)(M O) (N O)))

(define (beginning-at x g)
  (let* [(b (let [(a '())]
              (for [(e (in-list g))]
                (cond [(equal? (car e) x)
                       (set! a (cons (cadr e) a))]))
              a))
         (c (apply append
                   (for/list [(e (in-list b))]
                     (beginning-at e g))))]
    (cons (list x)
          (map (lambda(e) (cons x e)) c))))
         
(define (ending-at x g)
  (map reverse (beginning-at x (map reverse g))))

(define member?
  (lambda (a lat)
    (cond ((null? lat) #f)
          (else (or (equal? (car lat) a)
                    (member? a (cdr lat)))))))
(define intersect
  (lambda (set1 set2)
    (cond ((null? set1) (quote ()))
          ((member? (car set1) set2)
           (cons (car set1)
                 (intersect (cdr set1) set2)))
          (else (intersect (cdr set1) set2)))))

(define (all-paths a b G)
  (intersect (beginning-at a G)
                (ending-at b G)))  

(define (duration lista)
  (for/sum [(x (in-list lista))] (dur x)))

(define (varianciaTotal lista)
  (for/sum [(x (in-list lista))] (durVariancia x)))

(define (durationPior lista)
  (for/sum [(x (in-list lista))] (dur2 x)))

(define (maximum pth [tmp '()])
  (cond [(null? pth) tmp]
        [(> (duration (car pth)) (duration tmp))
         (maximum (cdr pth) (car pth))]
        [else (maximum (cdr pth) tmp)]))

(define (CP elem1 elem2 lista)
  (maximum (all-paths elem1 elem2 lista)))

(define (min elem1 elem2 g)
  (let [(caminho (all-paths elem1 elem2 g))]
    (if (null? caminho) '()
        (let loop [(lista (cdr caminho)) (min (duration (car caminho))) (minCam (car caminho))]
          (cond [(null? lista) (list minCam min)]
                [(< (duration (car lista)) min) (loop (cdr lista) (duration (car lista)) (car lista))]
                [else (loop (cdr lista) min minCam)])))))
            
(define (max elem1 elem2 g)
  (let [(caminho (all-paths elem1 elem2 g))]
    (if (null? caminho) '()
        (let loop [(lista (cdr caminho)) (min (duration (car caminho))) (minCam (car caminho))]
          (cond [(null? lista) (list minCam min)]
                [(> (duration (car lista)) min) (loop (cdr lista) (duration (car lista)) (car lista))]
                [else (loop (cdr lista) min minCam)])))))

(define (tempos elem1 elem2 g var)
  (let [(gE 
         (for/list [(e (in-list var))]
           ;(displayln e)
           (let* [(ESm (CP elem1 e g))
                  (ES (duration (take ESm (- (length ESm) 1))))]
             ;(displayln ES)
             (list e ES (+ ES (dur e))))))]
    ;(displayln gE)
    (let [(gL 
           (for/list [(e (in-list var))]
             ;(displayln e)
             (let [(L (duration (CP elem1 elem2 g)))
                    (a (CP e elem2 g))]
               (cond [(null? a) (set! a '(1 2))])
                   (let [(LF (- L (duration (cdr (take a (- (length a) 1))))))]
                     ;(displayln (list e LF (- LF (dur e))))
                     (list e LF (- LF (dur e)))
               ))))]
      ;(displayln gL)
      (let [(s
             (call-with-output-string 
              (lambda(p)
                (displayln "<table>
  <tr>
    <th>Tarefa</th>
    <th>Tempo Inicial Mais Cedo</th>
    <th>Tempo Inicial Mais Tarde</th>
    <th>Tempo Final Mais Cedo</th>
    <th>Tempo Final Mais Tarde</th>
    <th>Sobra</th>
  </tr>" p)
                (for [(a (in-list gE))
                      (b (in-list gL))]
                  (match a
                    [(list el x y)
                     (match b 
                       [(list el z v)
                        (displayln "<tr><td>" p)
                        (displayln (~a  el  ) p)
                        (displayln "</td><td>" p)
                        (displayln (~r  x  #:precision 2) p)
                        (displayln "</td><td>" p)
                        (displayln (~r  v  #:precision 2) p)
                        (displayln "</td><td>" p)
                        (displayln (~r y   #:precision 2) p)
                        (displayln "</td><td>" p)
                        (displayln (~r  z  #:precision 2) p)
                        (displayln "</td><td>" p)
                        (displayln (~r (- z y)  #:precision 2) p)
                        (displayln "</td></tr>" p )
                        ])]))
                (displayln "</table>" p)
                )))]
        ;(displayln s)
        s
        )
      )
    ))

(define (maxPiorCaso pth [tmp '()])
  (cond [(null? pth) tmp]
        [(> (durationPior (car pth)) (durationPior tmp))
         (maxPiorCaso (cdr pth) (car pth))]
        [else (maxPiorCaso (cdr pth) tmp)]))

(define (piorCaminho elem1 elem2 g)
  (let [(cp (maxPiorCaso (all-paths elem1 elem2 g)))]
    ;(displayln cp)
    (durationPior cp)))


(define (probabilidadeTempo elem1 elem2 g d)
  (let [(u (duration (CP elem1 elem2 g)))
        (v (sqrt (varianciaTotal (CP elem1 elem2 g))))]
    (let* [(fx (lambda(x)
                 (* (/ 1 v (sqrt (* 2 pi))) (exp (* (- (/ 1 2)) (sqr (/ (- x u) v)))))))
          (integralF (chebev (chint (chebft 0 
                                            (piorCaminho elem1 elem2 g) 
                                            32 fx))))]
      ;(plot (function fx -5 50))
          (integralF d)
      ;(integralF d)
      )))

(define (geraJS inicio fim var G)
  (call-with-output-file "cpmOut.html"
    #:exists 'replace #:mode 'text
    (lambda(p)
      (displayln "<!doctype html>
<html>
<head>
  <title>Resultado PERT-CPM</title>
  <meta http-equiv='Content-Type' content='text/html;charset=utf-8' >	
  <script type='text/javascript' src='vis.js'></script>
  <link href='vis.css' rel='stylesheet' type='text/css' />

  <style type='text/css'>
    #mynetwork {
      width: 1250px;
      height: 400px;
      border: 1px solid lightgray;
    }
    table {
    width:100%;
   }
   th, td {
      padding: 5px;
      text-align: left;
   }
   table tr:nth-child(even) {
      background-color: #eee;
   }
   table tr:nth-child(odd) {
      background-color:#fff;
   }
  table th	{
      background-color: black;
      color: white;
  }
  IMG.displayed {
    display: block;
    margin-left: auto;
    margin-right: auto }
  </style>
</head>
<body>

<h1>
  Resultado da execução de algoritmo Pert-Cpm.
</h1>

<div id='mynetwork'></div>

<script type='text/javascript'>
var nodes = new vis.DataSet([" p)
      (for [(info (in-list var))
            (i (in-range 1 (+ (length var) 1)))]
        (displayln (~a "{id: " i ",label: '" info "'},") p))
     (displayln " ]);
var edges = new vis.DataSet([" p)
      (for [(info (in-list G))]
        (match info
          [(list a b)
           (displayln (~a "{from: " (posicao a var) ", to: " (posicao b var) ", dashes:false},") p)           
           ]))
      (displayln " ]);" p)
      (displayln "var container = document.getElementById('mynetwork');
  var data = {
    nodes: nodes,
    edges: edges
  };
  var options = {};
  var network = new vis.Network(container, data, options);
</script>" p)
      (displayln (~a "<p>" (maxPiorCaso (all-paths inicio fim G)) "</p>") p)
      (displayln (tempos inicio fim G var) p)
      (graficoFinal inicio fim G)
      (displayln "<h2>Gráfico de Probabilidade de Termino</h2>" p)
      (displayln (~a "<img id='my_image' src='cpmGrafico.jpg' alt='Probabilidade de Termino' align='center'") p)
      (displayln "</body>
</html>" p)
      ))
  )

(define (graficoFinal inicio fim G)
  (let [(vetor
         (for/list [(dia (in-range (+ (piorCaminho inicio fim G) 1)))]
           (vector dia (* 100 (probabilidadeTempo inicio fim G dia)))))]
    (plot-file (list (axes)
                (points vetor #:size 2))
           #:y-min 0 #:y-max 100  #:x-min 0 #:x-max (+ (piorCaminho inicio fim G) 2)
           #:x-label "Semana" #:y-label "Probabilidade(%)"
           #:title "Probabilidade de termino em até"
            "cpmGrafico.jpg" 
          )))

(define (posicao a lista)
  (let [(l 0)]
    (for [(b (in-list lista))
        (i (in-range 1 (+ (length lista) 1)))]
    (when (equal? a b) ;(displayln i)
           (set! l i))                        
          )
    l)
  )
#|> (probabilidadeTempo 'A 'O G 47)
0.8411629781011529
> (probabilidadeTempo 'A 'O G 44)
0.49977618931430007
> 
> (piorCaminho 'A 'O G)
> 70
> (tempos 'A 'O G var)
Tarefa: A ES: 0 EF: 2 LS: 0.0  LF: 2.0 S: 0.0
Tarefa: B ES: 2 EF: 6.0 LS: 2.0  LF: 6.0 S: 0.0
Tarefa: C ES: 6.0 EF: 16.0 LS: 6.0  LF: 16.0 S: 0.0
Tarefa: D ES: 16.0 EF: 22.0 LS: 20.0  LF: 26.0 S: 4.0
Tarefa: E ES: 16.0 EF: 20.0 LS: 16.0  LF: 20.0 S: 0.0
Tarefa: F ES: 20.0 EF: 25.0 LS: 20.0  LF: 25.0 S: 0.0
Tarefa: G ES: 22.0 EF: 29.0 LS: 26.0  LF: 33.0 S: 4.0
Tarefa: H ES: 29.0 EF: 38.0 LS: 33.0  LF: 42.0 S: 4.0
Tarefa: I ES: 16.0 EF: 23.0 LS: 18.0  LF: 25.0 S: 2.0
Tarefa: J ES: 25.0 EF: 33.0 LS: 25.0  LF: 33.0 S: 0.0
Tarefa: K ES: 33.0 EF: 37.0 LS: 34.0  LF: 38.0 S: 1.0
Tarefa: L ES: 33.0 EF: 38.0 LS: 33.0  LF: 38.0 S: 0.0
Tarefa: M ES: 38.0 EF: 40.0 LS: 42.0  LF: 44.0 S: 4.0
Tarefa: N ES: 38.0 EF: 44.0 LS: 38.0  LF: 44.0 S: 0.0
Tarefa: O ES: 44.0 EF: 44.0 LS: 44.0  LF: 44.0 S: 0.0
> 
|#

  