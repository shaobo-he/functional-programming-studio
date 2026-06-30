#lang racket

(require "sudoku.rkt")

(provide (all-defined-out))

;; re-implementation of http://norvig.com/sudoku.html

;(define size (cons 3 3))

(define get/units
  (λ (indices size)
    (let-indices ([(row-num col-num) size])
                 (let-indices ([(row-index col-index) indices])
                              (let ([board-size (* row-num col-num)])
                                `(,(get/row-indices row-index board-size)
                                  ,(get/col-indices col-index board-size)
                                  ,(get/square-indices indices size)))))))

(define get/peers
  (λ (indices size)
    (set->list (set-remove (list->set (let ([all (get/units indices size)])
                                        (append (first all) (append (second all) (third all)))))
                           indices))))

(define get/indices (λ (X Y) (combine (build-list X (λ (x) x)) (build-list Y (λ (x) x)))))

(define solved-grid?
  (λ (grid size)
    (let ([board-size (* (car size) (cdr size))])
      (andmap (λ (indices) (= (length (hash-ref grid indices)) 1))
              (get/indices board-size board-size)))))

(define solved-grid->string
  (λ (grid size)
    (let ([board-size (* (car size) (cdr size))])
      (foldr
       (λ (row str)
         (string-append
          (foldr (λ (col str)
                   (string-append (number->string (car (hash-ref grid (cons row col)))) " " str))
                 ""
                 (build-list board-size (λ (x) x)))
          "\n"
          str))
       ""
       (build-list board-size (λ (x) x))))))

(define board->grid
  (λ (board size)
    (let-indices
     ([(row-num col-num) (get/board-size board)])
     (let ([pos (get/indices row-num col-num)])
       (let ([initial-grid (foldl (λ (indices grid)
                                    (hash-set grid indices (build-list row-num (λ (x) (+ x 1)))))
                                  (hash (cons 0 0) '(0))
                                  pos)])
         (foldl (λ (indices grid)
                  (if (false? grid)
                      #f
                      (let ([board-pos (get/pos board indices)])
                        (if (not (Hole? board-pos))
                            (assign grid size indices board-pos)
                            grid))))
                initial-grid
                pos))))))

(define assign
  (λ (grid size indices val)
    (let ([other-values (remove val (hash-ref grid indices))])
      (foldl (λ (option grid)
               (if (false? grid)
                   #f
                   (eliminate grid size indices option)))
             grid
             other-values))))

(define eliminate
  (λ (grid size indices val)
    (letrec ([apply/eliminate-rule-1 (λ (grid val)
                                       (foldl (λ (peer grid)
                                                (if (false? grid)
                                                    #f
                                                    (eliminate grid size peer val)))
                                              grid
                                              (get/peers indices size)))]
             [apply/eliminate-rule-2
              (λ (grid)
                (foldl (λ (unit grid)
                         (if (false? grid)
                             #f
                             (let ([possible-indices
                                    (filter (λ (indices) (member val (hash-ref grid indices))) unit)])
                               (cond
                                 [(empty? possible-indices) #f]
                                 [(= (length possible-indices) 1)
                                  (assign grid size (car possible-indices) val)]
                                 [else grid]))))
                       grid
                       (get/units indices size)))])
      (let ([options (hash-ref grid indices)])
        (if (false? (member val options))
            grid
            (let ([eliminated-options (remove val options)])
              (let ([grid (hash-set grid indices eliminated-options)])
                (if (empty? eliminated-options)
                    #f
                    (let ([grid-after-rule-1 (if (= (length eliminated-options) 1)
                                                 (apply/eliminate-rule-1 grid
                                                                         (car eliminated-options))
                                                 grid)])
                      ;; Maybe Monad wanted!!!
                      (if (false? grid-after-rule-1)
                          #f
                          (apply/eliminate-rule-2 grid-after-rule-1)))))))))))

(define solve-cp
  (λ (board size)
    (call/cc (λ (k)
               (let ([board-size (* (car size) (cdr size))])
                 (letrec ([solve-cp-grid
                           (λ (grid)
                             (if (solved-grid? grid size)
                                 (k grid)
                                 (let ([best-option (argmin (λ (indices)
                                                              (let ([len (length (hash-ref grid
                                                                                           indices))])
                                                                (if (> len 1)
                                                                    len
                                                                    (+ board-size 1))))
                                                            (get/indices board-size board-size))])
                                   (foldl (λ (val result)
                                            (let ([new-grid (assign grid size best-option val)])
                                              (if (false? new-grid)
                                                  #f
                                                  (solve-cp-grid new-grid))))
                                          #f
                                          (hash-ref grid best-option)))))])
                   (let ([init-grid (board->grid board size)])
                     (if (false? init-grid)
                         #f
                         (solve-cp-grid init-grid)))))))))
