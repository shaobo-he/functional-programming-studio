#lang racket

(require rackunit
         "../sudoku.rkt")

(define parsed-board (make/board "1 0\n0 2"))

(check-equal? parsed-board (list (list 1 (Hole 0)) (list (Hole 0) 2)))
(check-false (make/board "1 nope\n0 2"))
(check-false (make/board "1 -1\n0 2"))
(check-equal? (get/board-size parsed-board) (cons 2 2))
(check-equal? (get/board-size '()) (cons 0 0))
(check-equal? (get/pos parsed-board (cons 1 0)) (Hole 0))

(define updated-board (set/pos parsed-board (cons 1 0) 1))

(check-equal? (get/pos updated-board (cons 1 0)) (Hole 1))
(check-equal? (get/pos parsed-board (cons 1 0)) (Hole 0))
(check-exn exn:fail? (λ () (set/pos parsed-board (cons 0 0) 2)))

(check-equal? (get/row-indices 2 3) (list (cons 2 0) (cons 2 1) (cons 2 2)))
(check-equal? (get/col-indices 2 3) (list (cons 0 2) (cons 1 2) (cons 2 2)))
(check-equal? (combine '(0 1) '(2 3))
              (list (cons 0 2) (cons 0 3) (cons 1 2) (cons 1 3)))
(check-equal? (get/square-indices (cons 4 7) (cons 3 3))
              (for*/list ([row (in-range 3 6)]
                          [col (in-range 6 9)])
                (cons row col)))

(define small-puzzle (make/board "1 0\n0 1"))

(check-true (conflict? small-puzzle (cons 1 2) (cons 0 1) 1))
(check-false (conflict? small-puzzle (cons 1 2) (cons 0 1) 2))
(check-equal? (solve small-puzzle (cons 1 2))
              (list (list 1 (Hole 2)) (list (Hole 2) 1)))
(check-equal? (with-output-to-string (λ () (display/board parsed-board)))
              "1 0 \n0 2 \n")
