#lang typed/racket

(require "sudoku.rkt")
(require "sudoku-cp.rkt")

(: make/real-board (-> String Board))
(define make/real-board
  (λ (s)
    (let ([board (make/board s)])
      (if (list? board) board (error "not a board")))))


(: display/real-board (-> (U False Board) Void))
(define display/real-board
  (λ (a)
    (if (false? a)
        (error "not valid board")
        (display/board a))))

(define empty-2by3 : Board
  (make/real-board
   "0 0 0 0 0 0
0 0 0 0 0 0
0 0 0 0 0 0
0 0 0 0 0 0
0 0 0 0 0 0
0 0 0 0 0 0"))

(define empty-3by3 : Board
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

(define leetcode-ex : Board 
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

(define not-fun : Board
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

(define not-solvable-small : Board
  (make/real-board
   "0 1
    2 0"))

(define simple : Board
  (make/real-board
   "1 0
0 0"))

(define the-most-difficult : Board
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