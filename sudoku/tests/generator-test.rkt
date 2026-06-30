#lang racket

(require rackunit
         "../sudoku.rkt"
         "../sudoku-cp.rkt"
         "../sudoku-gen.rkt")

(define size (cons 2 2))
(define empty-grid (build/empty-grid size))

(check-equal? (hash-count empty-grid) 16)
(check-true (for/and ([cell (in-list (get/indices 4 4))])
              (equal? (hash-ref empty-grid cell) '(1 2 3 4))))
(check-equal? (count/total-options empty-grid size) 64)

(define partial-grid (hash-set empty-grid (cons 0 0) '(3)))
(define partial-board (grid->board partial-grid size))

(check-equal? (get/pos partial-board (cons 0 0)) 3)
(check-equal? (get/pos partial-board (cons 0 1)) (Hole 0))
(check-equal? (get/board-size partial-board) (cons 4 4))

(define generated-board
  (parameterize ([current-pseudo-random-generator
                  (make-pseudo-random-generator)])
    (random-seed 20260630)
    (generate size)))
(define generated-solution (solve-cp generated-board size))

(check-equal? (get/board-size generated-board) (cons 4 4))
(check-not-false generated-solution)
(check-true (solved-grid? generated-solution size))
