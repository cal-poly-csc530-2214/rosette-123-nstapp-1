#lang racket

(require "solver.rkt" "graph.rkt")

(provide
 k-coloring      ; (->* (graph/c natural-number/c) [(is-a? solver%)] (or/c #f coloring/c))
 valid-coloring? ; (-> graph/c coloring/c boolean?)
)

; Returns true iff the given coloring is correct for the specified graph.
(define (valid-coloring? graph coloring)
  (and (coloring/c coloring)
       (= (color-size coloring) (node-count graph))
       (for*/and ([(e n) (in-indexed graph)] [child e])
         (not (= (color-ref coloring n) (color-ref coloring child))))))

; Returns a coloring/c if the given graph can 
; be colored with k colors.  Otherwise returns #f.
(define (k-coloring graph k)
  (define length (vector-length graph))
  (define atleast (for/list ([i (in-range 1 (+ length 1))])
    (for/list ([j (in-range (+ (* (- i 1) k) 1) (+ (* i k) 1))])
    j)))
  (define atmost (append atleast (apply append (map (lambda (inner)
                      (combinations (map (lambda (i) (* -1 i)) inner) 2)) atleast))))
  (define cnf (append atmost (apply append (apply append (for/list ([v graph]
             [i (in-naturals)])
    (for/list ([j v]) (for/list ([l (in-range (+ (* i k) 1) (+ (* (+ i 1) k) 1))])
                        (list (* l -1) (* (+ (* (- j i) k) l) -1)
                        ))))))))
  (define coloring (solve cnf))
  (match coloring
          [#f #f]
          [_ (list->vector (map (lambda (color) (modulo color 4)) (filter positive? coloring)))]))
