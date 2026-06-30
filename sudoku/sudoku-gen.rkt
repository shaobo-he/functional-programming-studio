#lang racket

(require "sudoku.rkt")
(require "sudoku-cp.rkt")

(define flip-coin
  (λ (n gen) (let ([b (= 0 (random n gen))]) (cons b (current-pseudo-random-generator)))))

#|
(define test-flip-coin
  (λ (n)
    (car
     (foldl
      (λ (n rs)
        (let* ([gen (cdr rs)]
               [p (flip-coin gen)])
          (cons (cons (car p) (car rs)) (cdr p))))
      (cons '() (begin (random-seed 1) (current-pseudo-random-generator)))
      (build-list n (λ (x) x))))))
|#

(define count/total-options
  (λ (grid size)
    (let ([board-size (* (car size) (cdr size))])
      (foldl (λ (indices sum) (+ sum (length (hash-ref grid indices))))
             0
             (get/indices board-size board-size)))))

(define build/empty-grid
  (λ (size)
    (let ([board-size (* (car size) (cdr size))])
      (let ([options (build-list board-size (λ (x) (+ x 1)))])
        (foldl (λ (indices grid) (hash-set grid indices options))
               (hash (cons 0 0) '())
               (get/indices board-size board-size))))))

(define grid->board
  (λ (grid size)
    (let ([board-size (* (car size) (cdr size))])
      (foldr (λ (row board)
               (cons (foldr (λ (col lst)
                              (cons (let ([pos (hash-ref grid (cons row col))])
                                      (if (= (length pos) 1)
                                          (car pos)
                                          (Hole 0)))
                                    lst))
                            '()
                            (build-list board-size (λ (x) x)))
                     board))
             '()
             (build-list board-size (λ (x) x))))))

(define generate
  (λ (size)
    (let ([board-size (* (car size) (cdr size))])
      (letrec ([generate-by-indices
                (λ (row-index col-index grid)
                  (cond
                    [(= col-index board-size) (generate-by-indices (+ 1 row-index) 0 grid)]
                    [(= row-index board-size) grid]
                    [else
                     (if (= 0 (random board-size))
                         (let* ([options (hash-ref grid (cons row-index col-index))]
                                [ts (foldl
                                     (λ (val ts)
                                       (let ([new-grid
                                              (assign grid size (cons row-index col-index) val)])
                                         (if (false? new-grid)
                                             ts
                                             (cons (cons val new-grid) ts))))
                                     '()
                                     options)])
                           (foldr (λ (ts grid)
                                    (if (false? grid)
                                        (generate-by-indices row-index (+ 1 col-index) (cdr ts))
                                        grid))
                                  #f
                                  (sort ts
                                        (λ (t1 t2)
                                          (< (count/total-options (cdr t1) size)
                                             (count/total-options (cdr t2) size))))))
                         (generate-by-indices row-index (+ 1 col-index) grid))]))])
        (let ([result (generate-by-indices 0 0 (build/empty-grid size))])
          (if (false? result)
              (error "fail to generate a board")
              (grid->board result size)))))))
