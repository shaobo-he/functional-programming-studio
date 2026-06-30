#lang racket

(require rackunit
         "../sudoku.rkt"
         "../sudoku-cp.rkt")

(define size (cons 3 3))
(define digits (range 1 10))
(define cells (get/indices 9 9))
(define open-grid
  (for/hash ([cell (in-list cells)])
    (values cell digits)))

(define center (cons 4 4))
(define center-units (get/units center size))
(define center-peers (get/peers center size))

(check-equal? (length center-units) 3)
(check-true (andmap (λ (unit) (= (length unit) 9)) center-units))
(check-equal? (length center-peers) 20)
(check-false (member center center-peers))
(check-equal? (length (remove-duplicates center-peers)) 20)

(define assigned-grid (assign open-grid size (cons 0 0) 1))

(check-not-false assigned-grid)
(check-equal? (hash-ref assigned-grid (cons 0 0)) '(1))
(for ([peer (in-list (get/peers (cons 0 0) size))])
  (check-false (member 1 (hash-ref assigned-grid peer))))
(check-not-false (member 1 (hash-ref assigned-grid (cons 4 4))))

(define singleton-grid (hash-set open-grid (cons 0 0) '(1)))

(check-false (eliminate singleton-grid size (cons 0 0) 1))
(check-equal? (eliminate open-grid size (cons 0 0) 10) open-grid)

;; Removing 1 from (0, 0) leaves (0, 1) as the only place for 1 in row 0.
(define hidden-single-grid
  (for/hash ([cell (in-list cells)])
    (values cell
            (cond
              [(equal? cell (cons 0 0)) '(1 2 3)]
              [(equal? cell (cons 0 1)) '(1 4)]
              [(zero? (car cell)) (range 2 10)]
              [else digits]))))
(define hidden-single-result (eliminate hidden-single-grid size (cons 0 0) 1))

(check-not-false hidden-single-result)
(check-equal? (hash-ref hidden-single-result (cons 0 1)) '(1))

(define leetcode-puzzle
  (make/board
   "5 3 0 0 7 0 0 0 0
6 0 0 1 9 5 0 0 0
0 9 8 0 0 0 0 6 0
8 0 0 0 6 0 0 0 3
4 0 0 8 0 3 0 0 1
7 0 0 0 2 0 0 0 6
0 6 0 0 0 0 2 8 0
0 0 0 4 1 9 0 0 5
0 0 0 0 8 0 0 7 9"))
(define expected-solution
  (list '(5 3 4 6 7 8 9 1 2)
        '(6 7 2 1 9 5 3 4 8)
        '(1 9 8 3 4 2 5 6 7)
        '(8 5 9 7 6 1 4 2 3)
        '(4 2 6 8 5 3 7 9 1)
        '(7 1 3 9 2 4 8 5 6)
        '(9 6 1 5 3 7 2 8 4)
        '(2 8 7 4 1 9 6 3 5)
        '(3 4 5 2 8 6 1 7 9)))
(define solved-grid (solve-cp leetcode-puzzle size))

(check-not-false solved-grid)
(check-true (solved-grid? solved-grid size))
(check-equal? (for/list ([row (in-range 9)])
                (for/list ([col (in-range 9)])
                  (car (hash-ref solved-grid (cons row col)))))
              expected-solution)

(define contradictory-puzzle
  (make/board
   "1 1 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0"))

(check-false (board->grid contradictory-puzzle size))
(check-false (solve-cp contradictory-puzzle size))
