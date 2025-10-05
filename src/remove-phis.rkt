#lang racket
(require 
  "dominator.rkt"
  "lang-util.rkt")

(provide 
  remove-phis)

(define (remove-phis program)
  (match program
    [(hash 'functions functions) 
     (hash-update program 'functions (curry map remove-func))]))

(define (remove-func func) 
  (define bbs (hash-ref func 'bbs))
  (define cfg (hash-ref func 'cfg))
  (define df  (get-dominator-frontier cfg))
  (define filtered-keys 
    (for/list ([bb (in-set (apply set-union (hash-values df)))]
               #:when (> (length (get-predecessors cfg bb)) 1))
      bb))
  (define types
    (for/hash ([lin (flatten (map (curry hash-ref bbs) filtered-keys))] 
               #:when (and (insn? lin) (ssa-get? (hash-ref lin 'op)))) 
      (values (hash-ref lin 'dest) (hash-ref lin 'type))))
  (hash-update func 'bbs (curryr remove-bbs types)))

(define (remove-bbs bbs types)
  (for/fold ([acc bbs]) 
    ([k (in-hash-keys bbs)]) 
    (hash-update acc k (λ (lins) (map (curryr remove-lin types) lins)))))

(define (remove-lin lin types)
  (match lin
    [(? label? lin) (remove-label lin types)]
    [(? insn?  lin) (remove-insn  lin types)]))

(define (remove-label label types)
  label)

(define (remove-insn insn types) 
  (match insn
    [(? effect?   insn) 
     (remove-effect   insn types)]
    [(? undef? insn) 
     (remove-undef    insn types)]
    [(? constant? insn) 
     (remove-constant insn types)]
    [(? value?    insn) 
     (remove-value    insn types)]))

(define (remove-effect effect types) 
  (define op (hash-ref effect 'op))
  (match op
    [(? ssa-set? op)
     (define args (hash-ref effect 'args))
     (define dest (first args))
     (define arg  (second args))
     (define type (hash-ref types dest))
     (hasheq 'type type 'dest dest 'op "id" 'args (list arg))]
    [else
      effect]))

(define (remove-undef undef types) 
  undef)

(define (remove-constant constant types) 
  constant)

(define (remove-value value types) 
  (define op (hash-ref value 'op))
  (match op
    [(? ssa-get? op)
     (hasheq 'op "nop")]
    [else
      value]))
