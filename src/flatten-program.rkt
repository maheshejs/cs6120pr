#lang racket
(require 
  racket/hash
  "lang-util.rkt")

(provide
 flatten-program)

(define (flatten-program program)
  (match program
    [(hash 'functions functions) 
     (hash-update program 'functions (curry map flatten-func))]))

(define (flatten-func func)
  (define bbs (hash-ref func 'bbs))
  (define sorted-keys (sort (hash-keys bbs) (λ (x y) (if (entry? x) 
                                                         #t
                                                         (if (entry? y) 
                                                           #f 
                                                           (< x y))))))
  (define instrs
    (for/fold ([acc '()]) 
      ([k sorted-keys])
      (append acc (hash-ref bbs k)))) 
  (hash-set (hash-filter-keys func (λ (k) (member k '(name args type)))) 'instrs instrs))

