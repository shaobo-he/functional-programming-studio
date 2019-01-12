#lang typed/racket

(provide (all-defined-out))

(struct Hole ([val : Natural]) #:transparent)

(define-type Pos (U Hole Natural))
(define-type Board (Listof (Listof Pos)))
(define-type Indices (Pairof Natural Natural))

(define-syntax-rule (let-indices ([(x y) indices]) body)
  (let ([x (car indices)]
        [y (cdr indices)]) body))

;; input format
;; for example, a 2*2 board
;; 1 0
;; 0 1
(: make/board (-> String (U False Board)))
(define make/board
  (λ (s)
    (letrec
        ([make/board-line : (-> (Listof String) (U False (Listof Pos)))
                          (λ (ss)
                            (cond
                              [(null? ss) '()]
                              [else (let ([n (string->number (car ss))])
                                      (cond
                                        [(exact-nonnegative-integer? n)
                                         (let ([rem (make/board-line (cdr ss))])
                                           (if (false? rem) #f (cons (if
                                                                      (zero? n)
                                                                      (Hole 0)
                                                                      n) rem)))]
                                        [else #f]))]))])
      (let ([result (foldl (λ ([line : String] [accm : (U False (Listof (Listof Pos)))])
                             (if (false? accm)
                                 #f
                                 (let ([line/ (make/board-line (string-split line))])
                                   (if (false? line/)
                                       #f
                                       (cons line/ accm))))) '() (string-split s "\n"))])
        (if (false? result)
            #f
            (reverse result))))))

(: display/board (-> Board Void))
(define display/board
  (λ (board)
    (let-indices ([(row-num col-num) (get/board-size board)])
                 (for ([row : Natural (in-range row-num)])
                   (begin
                     (for ([col : Natural (in-range col-num)])
                       (let ([pos (get/pos board (cons row col))])
                         (begin
                           (display (if (Hole? pos)
                                        (Hole-val pos)
                                        pos))
                           (display " "))))
                     (display "\n"))))))

(: get/board-size (-> Board Indices))
(define get/board-size
  (λ (board)
    (let ([row-num (length board)])
      (if (= row-num 0)
          (cons 0 0)
          (cons row-num (length (list-ref board 0)))))))

(: get/pos (-> Board Indices Pos))
(define get/pos
  (λ (board indices)
    (let-indices ([(row-index col-index) indices])
                 (list-ref (list-ref board row-index) col-index))))

(: set/pos (-> Board Indices Natural Board))
(define set/pos
  (λ (board indices val)
    (let ([pos (get/pos board indices)])
      (let-indices
       ([(row-index col-index) indices])
       (cond
         [(Hole? pos) (list-set board row-index
                                (list-set (list-ref board row-index) col-index
                                          (Hole val)))]
         [else (error "cannot set a fixed position")])))))

(: get/row-indices (-> Natural Natural (Listof Indices)))
(define get/row-indices
  (λ (row-index col-size)
    (build-list col-size (λ ([col : Natural]) (cons row-index col)))))

(: get/col-indices (-> Natural Natural (Listof Indices)))
(define get/col-indices
  (λ (col-index row-size)
    (build-list row-size (λ ([row : Natural]) (cons row col-index)))))

;; I couldn't get for/list working in typed racket
(: combine (-> (Listof Natural) (Listof Natural) (Listof Indices)))
(define combine
  (λ (rows cols)
    (cond
      [(null? rows) '()]
      [else (append
             (map (λ ([col : Natural])
                    (cons (car rows) col))
                  cols)
             (combine (cdr rows) cols))])))

(: get/square-indices (-> Indices Indices (Listof Indices)))
(define get/square-indices
  (λ (indices size)
    (let-indices
     ([(X Y) size])
     (let-indices
      ([(row-index col-index) indices])
      (combine (build-list X (λ ([x : Natural])
                               (+ (* X (quotient row-index X)) x)))
               (build-list Y (λ ([y : Natural])
                               (+ (* Y (quotient col-index Y)) y))))))))

(: conflict? (-> Board Indices Indices Natural Boolean))
(define conflict?
  (λ (board size indices val)
    (let-indices
     ([(row-num col-num) (get/board-size board)])
     (let-indices
      ([(row-index col-index) indices])
      (let ([any? (λ ([lst : (Listof Boolean)])
                    (ormap (λ ([x : Boolean]) x) lst))]
            [pos-eq? (λ ([indices : Indices])
                       (let ([pos (get/pos board indices)])
                         (if (Hole? pos)
                             (= (Hole-val pos) val)
                             (= pos val))))])
        (any? `(,(any? (map pos-eq? (get/row-indices row-index col-num)))
                ,(any? (map pos-eq? (get/col-indices col-index row-num)))
                ,(any? (map pos-eq? (get/square-indices indices size))))))))))

(: solve (-> Board Indices (U False Board)))
(define solve
  (λ (board size)
    (call/cc (λ ([k : (-> Board Board)])
               (let-indices
                ([(row-num col-num) (get/board-size board)])
                (letrec ([solve-by-indices : (-> Board Indices (U False Board))
                                           (λ (board indices)
                                             (let-indices ([(row-index col-index) indices])
                                                          (cond
                                                            ;; next row
                                                            [(= col-index col-num) (solve-by-indices board (cons (+ row-index 1) 0))]
                                                            ;; row exhausted
                                                            [(= row-index row-num) (k board)]
                                                            [else
                                                             (let ([pos (get/pos board indices)])
                                                               (if (and (Hole? pos) (= (Hole-val pos) 0))
                                                                   (foldl
                                                                    (λ ([val : Natural] [result : (U False Board)])
                                                                      (if (not (conflict? board size indices val))
                                                                          (solve-by-indices (set/pos board indices val)
                                                                                            (cons row-index (+ col-index 1)))
                                                                          #f))
                                                                    #f
                                                                    (build-list row-num (λ ([x : Natural]) (+ x 1))))
                                                                   (solve-by-indices board (cons row-index (+ col-index 1)))))])))])                                                                                   
                  (solve-by-indices board (cons 0 0))))))))

#|
(: generate (-> Indices Board))
(define generate
  (λ (size)
    (let-indices
     ([(X Y) size])
     (let ()))))
|#