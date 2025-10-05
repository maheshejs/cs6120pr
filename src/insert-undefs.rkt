#lang racket
(require 
  racket/hash
  "lang-util.rkt")

(provide 
  insert-undefs)

(define (insert-undefs program)
  (match program
    [(hash 'functions functions) 
     (hash-update program 'functions (curry map insert-func))]))

(define (insert-func func) 
  (define defs (hash-ref func 'defs))
  (hash-update func 'bbs (curryr insert-bbs defs)))

(define (insert-bbs bbs defs)
  (hash-update bbs 'entry (curryr insert-entry defs)))

(define (insert-entry lins defs)
  (define filtered-defs (hash-filter defs (λ (k v) (not (set-member? v 'entry)))))
  (cons (car lins) 
        (for/fold ([acc (cdr lins)]) 
          ([type-var (in-hash-keys filtered-defs)]) 
          (cons (hasheq 'type (car type-var) 'dest (cdr type-var) 'op "undef") acc))))
