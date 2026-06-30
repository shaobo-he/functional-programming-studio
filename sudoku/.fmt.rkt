#lang racket/base

(require fmt/conventions)

(provide the-formatter-map)

(define (the-formatter-map form)
  (case form
    [("let-indices") (standard-formatter-map "let")]
    [else #f]))
