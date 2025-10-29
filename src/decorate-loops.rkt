#lang racket
(require 
  racket/hash
  "dominator.rkt"
  "lang-util.rkt")

(provide 
  decorate-loops)

(define (decorate-loops program)
  (match program
    [(hash 'functions functions) 
     (hash-update program 'functions (curry map decorate-func))]))

(define (decorate-func func) 
  (define cfg (hash-ref func 'cfg))
  (define loops (get-natural-loops cfg))
  (hash-set (hash-set func 'loops loops) 'taints (hash)))
