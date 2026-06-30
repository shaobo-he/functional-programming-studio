#lang racket

(require "sudoku.rkt")
(require "sudoku-cp.rkt")

(define (make/real-board s)
  (let ([board (make/board s)])
    (if (list? board)
        board
        (error "not a board"))))

(define (display/real-board a)
  (if (false? a)
      (error "not a valid board")
      (display/board a)))

(define (display/real-grid g size)
  (if (false? g)
      (error "not a valid board")
      (display (solved-grid->string g size))))

(define empty-2by3
  (make/real-board
   "0 0 0 0 0 0
0 0 0 0 0 0
0 0 0 0 0 0
0 0 0 0 0 0
0 0 0 0 0 0
0 0 0 0 0 0"))

(define empty-3by3
  (make/real-board
   "0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0"))

(define leetcode-ex
  (make/real-board
   "5 3 0 0 7 0 0 0 0
6 0 0 1 9 5 0 0 0
0 9 8 0 0 0 0 6 0
8 0 0 0 6 0 0 0 3
4 0 0 8 0 3 0 0 1
7 0 0 0 2 0 0 0 6
0 6 0 0 0 0 2 8 0
0 0 0 4 1 9 0 0 5
0 0 0 0 8 0 0 7 9"))

(define not-fun
  (make/real-board
   "0 2 0 0 0 0 0 0 0
0 0 0 6 0 0 0 0 3
0 7 4 0 8 0 0 0 0
0 0 0 0 0 3 0 0 2
0 8 0 0 4 0 0 1 0
6 0 0 5 0 0 0 0 0
0 0 0 0 1 0 7 8 0
5 0 0 0 0 9 0 0 0
0 0 0 0 0 0 0 4 0"))

(define not-solvable-small (make/real-board "0 1
    2 0"))

(define simple (make/real-board "1 0
0 0"))

(define the-most-difficult-known
  (make/real-board
   "8 5 0 0 0 2 4 0 0
7 2 0 0 0 0 0 0 9
0 0 4 0 0 0 0 0 0
0 0 0 1 0 7 0 0 2
3 0 5 0 0 0 9 0 0
0 4 0 0 0 0 0 0 0
0 0 0 0 8 0 0 7 0
0 1 7 0 0 0 0 0 0
0 0 0 0 3 6 0 4 0
"))

(define the-most-difficult-created
  (make/real-board
   "0 0 5 3 0 0 0 0 0
8 0 0 0 0 0 0 2 0
0 7 0 0 1 0 5 0 0
4 0 0 0 0 5 3 0 0
0 1 0 0 7 0 0 0 6
0 0 3 2 0 0 0 8 0
0 6 0 5 0 0 0 0 9
0 0 4 0 0 0 0 3 0
0 0 0 0 0 9 7 0 0
"))

(define near-worst-case
  (make/real-board
   "0 0 0 0 0 0 0 0 0
0 0 0 0 0 3 0 8 5
0 0 1 0 2 0 0 0 0
0 0 0 5 0 7 0 0 0
0 0 4 0 0 0 1 0 0
0 9 0 0 0 0 0 0 0
5 0 0 0 0 0 0 7 3
0 0 2 0 1 0 0 0 0
0 0 0 0 4 0 0 0 9"))
