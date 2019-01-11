#lang typed/racket

(require "sudoku.rkt")

;; re-implementation of http://norvig.com/sudoku.html

(define-type Options (Listof Natural))
(define-type Grid (Immutable-HashTable Indices Options))
(define-type MaybeGrid (U False Grid))

(define-type Unit (Listof Indices))

(define size (cons 3 3))

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


(: board->grid (-> Board Indices (U False Grid)))
(define board->grid
  (λ (board size)
    (let-indices
     ([(row-num col-num) (get/board-size board)])
     (let ([pos : (Listof Indices) (combine (build-list row-num (λ ([x : Natural]) x))
                         (build-list col-num (λ ([x : Natural]) x)))])
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
    (foldl
     (λ ([option : Natural] [grid : MaybeGrid])
       (if (false? grid)
           #f
           (eliminate grid indices size option)))
     grid
     (remove val (hash-ref grid indices)))))

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
              (if (empty? eliminated-options)
                  #f
                  (let ([grid-after-rule-1
                         (if (= (length eliminated-options) 1)
                             (apply/eliminate-rule-1 grid (car eliminated-options))
                             grid)])
                    ;; Maybe Monad wanted!!!
                    (if (false? grid-after-rule-1)
                        #f
                        (apply/eliminate-rule-2 grid-after-rule-1))))))))))