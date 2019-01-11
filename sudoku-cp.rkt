#lang typed/racket

(require "sudoku.rkt")

;; re-implementation of http://norvig.com/sudoku.html

(define-type Options (Listof Natural))
(define-type Grid (Immutable-HashTable Indices (Listof Options)))

(define-type Unit (Listof Indices))

(define size (cons 3 3))

(: get/units (-> Indices (Listof Unit)))
(define get/units
  (λ (indices)
    (let-indices
     ([(row-num col-num) size])
     (let-indices
      ([(row-index col-index) indices])
      (let ([board-size (* row-num col-num)])
        `(, (get/row-indices row-index board-size)
          , (get/col-indices col-index board-size)
          , (get/square-indices indices size)))))))


(: get/peers (-> Indices (Listof Indices)))
(define get/peers
  (λ (indices)
    (set->list
     (set-remove
      (list->set (let ([all (get/units indices)])
                   (append (first all)
                           (append (second all)
                                   (third all)))))
      indices))))

(: board->grid (-> Board Grid))
(define board->grid
  (λ (board)
    ()))