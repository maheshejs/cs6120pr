#lang racket
(require 
  graph
  "lang-util.rkt")

(provide 
  fake-entry)

(define (fake-entry program)
  (match program
    [(hash 'functions functions) 
     (hash-update program 'functions (curry map fake-func))]))

(define (fake-func func) 
  (define cfg (hash-ref func 'cfg))
  (add-directed-edge! cfg 'entry 0)
  (hash-update (hash-update func 'cfg (const cfg)) 'bbs fake-bbs))

(define (fake-bbs bbs)
  (hash-update (hash-set bbs 'entry (list (hasheq 'label "ssa.entry") 
                                          (hasheq 'op "jmp" 'labels (list "ssa.first")))) 
               0 
               (λ (lins) (cons (hasheq 'label "ssa.first") lins))))

