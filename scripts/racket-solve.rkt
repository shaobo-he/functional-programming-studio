#lang racket

(require "../sudoku.rkt")
(require "../sudoku-cp.rkt")

(define input (port->string (current-input-port)))
(define board (make/board input))

(cond
  [(not board) (display "false\n")]
  [else
   (define solved (solve-cp board (cons 3 3)))
   (if solved
       (display (solved-grid->string solved (cons 3 3)))
       (display "false\n"))])
