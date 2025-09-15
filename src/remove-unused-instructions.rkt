#lang racket
(require 
  graph 
  racket/hash
  "lang-util.rkt")

(provide
 remove-unused-instructions)

(define (remove-unused-instructions program)
  (match program
    [(hash 'functions functions) 
     (hash-update program 'functions (curry map remove-func))]))

(define (remove-func func)
  (define cfg (hash-ref func 'cfg))
  (define-values (h _) (bfs cfg 0))
  (define fh (hash-filter-values h (lambda (x) (infinite? x))))
  (define bbs
    (for/fold ([acc (hash-ref func 'bbs)]) 
      ([k (in-hash-keys fh)])
      (hash-remove acc k)))
  (for ([k (in-hash-keys fh)])
    (remove-vertex! cfg k))
  (hash-update (hash-update func 'bbs (const bbs)) 'cfg (const cfg)))
