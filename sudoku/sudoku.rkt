#lang racket

(provide (all-defined-out))

(struct Hole (val) #:transparent)

(define-syntax-rule (let-indices ([(x y) indices])
                      body ...)
  (let ([x (car indices)]
        [y (cdr indices)])
    body ...))

;; input format
;; for example, a 2*2 board
;; 1 0
;; 0 1
(define (make/board s)
  (define (make/board-line ss)
    (cond
      [(null? ss) '()]
      [else
       (let ([n (string->number (car ss))])
         (cond
           [(exact-nonnegative-integer? n)
            (let ([rem (make/board-line (cdr ss))])
              (if (false? rem)
                  #f
                  (cons (if (zero? n)
                            (Hole 0)
                            n)
                        rem)))]
           [else #f]))]))
  (let ([result
         (foldl (λ (line accm)
                  (if (false? accm)
                      #f
                      (let ([line/ (make/board-line (string-split line))])
                        (if (false? line/)
                            #f
                            (cons line/ accm)))))
                '()
                (string-split s "\n"))])
    (if (false? result)
        #f
        (reverse result))))

(define (display/board board)
  (let-indices ([(row-num col-num) (get/board-size board)])
    (for ([row (in-range row-num)])
      (for ([col (in-range col-num)])
        (let ([pos (get/pos board (cons row col))])
          (display (if (Hole? pos)
                       (Hole-val pos)
                       pos))
          (display " ")))
      (display "\n"))))

(define (get/board-size board)
  (let ([row-num (length board)])
    (if (= row-num 0)
        (cons 0 0)
        (cons row-num (length (list-ref board 0))))))

(define (get/pos board indices)
  (let-indices ([(row-index col-index) indices])
    (list-ref (list-ref board row-index) col-index)))

(define (set/pos board indices val)
  (let ([pos (get/pos board indices)])
    (let-indices ([(row-index col-index) indices])
      (cond
        [(Hole? pos)
         (list-set board
                   row-index
                   (list-set (list-ref board row-index) col-index (Hole val)))]
        [else (error "cannot set a fixed position")]))))

(define (get/row-indices row-index col-size)
  (build-list col-size (λ (col) (cons row-index col))))

(define (get/col-indices col-index row-size)
  (build-list row-size (λ (row) (cons row col-index))))

(define (combine rows cols)
  (cond
    [(null? rows) '()]
    [else
     (append (map (λ (col) (cons (car rows) col)) cols)
             (combine (cdr rows) cols))]))

(define (get/square-indices indices size)
  (let-indices ([(X Y) size])
    (let-indices ([(row-index col-index) indices])
      (combine (build-list X (λ (x) (+ (* X (quotient row-index X)) x)))
               (build-list Y (λ (y) (+ (* Y (quotient col-index Y)) y)))))))

(define (conflict? board size indices val)
  (define (any? lst)
    (ormap (λ (x) x) lst))
  (define (pos-eq? indices)
    (let ([pos (get/pos board indices)])
      (if (Hole? pos)
          (= (Hole-val pos) val)
          (= pos val))))
  (let-indices ([(row-num col-num) (get/board-size board)])
    (let-indices ([(row-index col-index) indices])
      (any? `(,(any? (map pos-eq? (get/row-indices row-index col-num)))
              ,(any? (map pos-eq? (get/col-indices col-index row-num)))
              ,(any? (map pos-eq? (get/square-indices indices size))))))))

(define (solve board size)
  (call/cc
   (λ (k)
     (let-indices ([(row-num col-num) (get/board-size board)])
       (define (solve-by-indices board indices)
         (let-indices ([(row-index col-index) indices])
           (cond
             ;; next row
             [(= col-index col-num)
              (solve-by-indices board (cons (+ row-index 1) 0))]
             ;; row exhausted
             [(= row-index row-num) (k board)]
             [else
              (let ([pos (get/pos board indices)])
                (if (and (Hole? pos) (= (Hole-val pos) 0))
                    (foldl (λ (val result)
                             (if (not (conflict? board size indices val))
                                 (solve-by-indices (set/pos board indices val)
                                                   (cons row-index
                                                         (+ col-index 1)))
                                 #f))
                           #f
                           (build-list row-num (λ (x) (+ x 1))))
                    (solve-by-indices board
                                      (cons row-index (+ col-index 1)))))])))
       (solve-by-indices board (cons 0 0))))))

#|
(define (generate size)
  (let-indices
   ([(X Y) size])
   (let ())))
|#
