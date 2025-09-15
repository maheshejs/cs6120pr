#lang racket
(require 
  "lang-util.rkt")

(provide
 flatten-program)

(define (flatten-program program)
  (match program
    [(hash 'functions functions) 
     (hash-update program 'functions (curry map flatten-func))]))

(define (flatten-func func)
  (define bbs (hash-ref func 'bbs))
  (define sorted-keys (sort (hash-keys bbs) <))
  (define instrs
    (for/fold ([acc '()]) 
      ([k sorted-keys])
      (append acc (hash-ref bbs k)))) 
  (hash-set (hash-remove (hash-remove func 'bbs) 'cfg) 'instrs instrs))
