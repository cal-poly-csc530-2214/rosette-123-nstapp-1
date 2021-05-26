#lang racket

(require "graph.rkt" "problems.rkt" "solver.rkt" "k-coloring.rkt")

; Extract all easy problems (see problems.rkt):
(define easy-problems
  (filter (lambda (p) 
          (eq? (problem-difficulty p) 'easy))
        problems))

; Print them
easy-problems

; Get the original test graph for this file
(define original-graph
  (first (filter (lambda (p) 
          (eq? (problem-difficulty p) 'easy)
          (< (problem-nodes p) 30)
          (< (problem-edges p) 30))
        problems)))

; Runs your k-coloring procedure (see k-coloring.rkt) on the provided problem, 
; printing timing data and #t/#f depending on whether the produced 
; coloring is valid or not. 
(define (run problem)
  (printf "---------~s---------\n" problem)
  (define graph (problem->graph problem))
  (define k (problem-colors problem))
  (define coloring (time (k-coloring graph k)))
  (display coloring)
  (printf "valid-coloring? ~a\n" (valid-coloring? graph coloring)))

; Run implementation on all easy graphs
;(for-each (lambda (problem) (run problem)) easy-problems)

; Run implementation on the original test graph
(run original-graph)