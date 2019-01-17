#lang typed/racket

(require "sudoku.rkt")

(provide (all-defined-out))

;; re-implementation of http://norvig.com/sudoku.html

(define-type Options (Listof Natural))
(define-type Grid (Immutable-HashTable Indices Options))
(define-type MaybeGrid (U False Grid))

(define-type Unit (Listof Indices))

;(define size (cons 3 3))

(: get/units (-> Indices Indices (Listof Unit)))
(define get/units
  (λ (indices size)
    (let-indices
     ([(row-num col-num) size])
     (let-indices
      ([(row-index col-index) indices])
      (let ([board-size (* row-num col-num)])
        `(, (get/row-indices row-index board-size)
          , (get/col-indices col-index board-size)
          , (get/square-indices indices size)))))))


(: get/peers (-> Indices Indices (Listof Indices)))
(define get/peers
  (λ (indices size)
    (set->list
     (set-remove
      (list->set (let ([all (get/units indices size)])
                   (append (first all)
                           (append (second all)
                                   (third all)))))
      indices))))

(: get/indices (-> Natural Natural (Listof Indices)))
(define get/indices
  (λ (X Y)
    (combine (build-list X (λ ([x : Natural]) x))
             (build-list Y (λ ([x : Natural]) x)))))

(: solved-grid? (-> Grid Indices Boolean))
(define solved-grid?
  (λ (grid size)
    (let ([board-size (* (car size) (cdr size))])
      (andmap (λ ([indices : Indices])
               (= (length (hash-ref grid indices)) 1))
               (get/indices board-size board-size)))))

(: solved-grid->string (-> Grid Indices String))
(define solved-grid->string
  (λ (grid size)
    (let ([board-size (* (car size) (cdr size))])
      (foldr
       (λ ([row : Natural] [str : String])
         (string-append
          (foldr
           (λ ([col : Natural] [str : String])
             (string-append
              (number->string (car (hash-ref grid (cons row col))))
              " "
              str))
           ""
           (build-list board-size (λ ([x : Natural]) x)))
          "\n"
          str))
       ""
       (build-list board-size (λ ([x : Natural]) x))))))

(: board->grid (-> Board Indices (U False Grid)))
(define board->grid
  (λ (board size)
    (let-indices
     ([(row-num col-num) (get/board-size board)])
     (let ([pos : (Listof Indices) (get/indices row-num col-num)])
       (let ([initial-grid : Grid (foldl (λ ([indices : Indices] [grid : Grid])
                                           (hash-set
                                            grid
                                            indices
                                            (build-list row-num (λ ([x : Natural]) (+ x 1)))))
                                         (hash (cons 0 0) '(0))
                                         pos)])
         (foldl (λ ([indices : Indices] [grid : (U False Grid)])
                  (if (false? grid)
                      #f
                      (let ([board-pos (get/pos board indices)])
                        (if (not (Hole? board-pos))
                            (assign grid size indices board-pos)
                            grid))))
                initial-grid
                pos))))))

(: assign (-> Grid Indices Indices Natural MaybeGrid))
(define assign
  (λ (grid size indices val)
    (let ([other-values (remove val (hash-ref grid indices))])
      (foldl
       (λ ([option : Natural] [grid : MaybeGrid])
         (if (false? grid)
             #f
             (eliminate grid size indices option)))
       grid
       other-values))))

(: eliminate (-> Grid Indices Indices Natural MaybeGrid))
(define eliminate
  (λ (grid size indices val)
    (letrec ([apply/eliminate-rule-1 : (-> Grid Natural MaybeGrid)
                                     (λ (grid val)
                                       (foldl
                                        (λ ([peer : Indices] [grid : MaybeGrid])
                                          (if (false? grid)
                                              #f
                                              (eliminate grid size peer val)))
                                        grid
                                        (get/peers indices size)))]
             [apply/eliminate-rule-2 : (-> Grid MaybeGrid)
                                     (λ (grid)
                                       (foldl
                                        (λ ([unit : Unit] [grid : MaybeGrid])
                                          (if (false? grid)
                                              #f
                                              (let ([possible-indices
                                                     (filter
                                                      (λ (indices)
                                                        (member val (hash-ref grid indices)))
                                                      unit)])
                                                (cond
                                                  [(empty? possible-indices) #f]
                                                  [(= (length possible-indices) 1) (assign grid size (car possible-indices) val)]
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
                    (let ([grid-after-rule-1
                           (if (= (length eliminated-options) 1)
                               (apply/eliminate-rule-1 grid (car eliminated-options))
                               grid)])
                      ;; Maybe Monad wanted!!!
                      (if (false? grid-after-rule-1)
                          #f
                          (apply/eliminate-rule-2 grid-after-rule-1)))))))))))

(: solve-cp (-> Board Indices MaybeGrid))
(define solve-cp
  (λ (board size)
    (call/cc (λ ([k : (-> Grid Grid)])
               (let ([board-size (* (car size) (cdr size))])
                 (letrec
                     ([solve-cp-grid : (-> Grid MaybeGrid)
                                     (λ (grid)
                                       (if (solved-grid? grid size)
                                           (k grid)
                                           (let ([best-option
                                                  (argmin
                                                   (λ (indices)
                                                     (let ([len (length (hash-ref grid indices))])
                                                       (if (> len 1) len (+ board-size 1))))
                                                   (get/indices board-size board-size))])
                                             (foldl (λ ([val : Natural] result)
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
