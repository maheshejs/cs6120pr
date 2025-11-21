#lang racket
(require 
  "lang-util.rkt")

(provide 
  trace-inline-calls)

(define (trace-inline-calls program traces)
  (match program
    [(hash 'functions functions) 
     (hash-update program 'functions (curry map (curryr inline-func traces)))]))

(define (inline-func func traces) 
  (define bbs (hash-ref func 'bbs))
  (hash-update func 'bbs (curryr inline-bbs traces)))

(define (inline-bbs bbs traces)
  (for/fold ([acc bbs]) 
    ([k (in-hash-keys bbs)]) 
    (hash-update acc k (λ (lins) (flatten (map (curryr inline-lin traces) lins))))))

(define (inline-lin lin traces)
  (match lin
    [(? label? lin) (inline-label lin traces)]
    [(? insn?  lin) (inline-insn  lin traces)]))

(define (inline-label label traces)
  (list label))

(define (inline-insn insn traces) 
  (match insn
    [(? effect?   insn) 
     (inline-effect   insn traces)]
    [(? undef? insn) 
     (inline-undef    insn traces)]
    [(? constant? insn) 
     (inline-constant insn traces)]
    [(? value?    insn) 
     (inline-value    insn traces)]))

(define (inline-effect effect traces) 
  (define op (hash-ref effect 'op))
  (match op
    [(? icall? op)
      (define name (first (hash-ref effect 'funcs)))
      (define func (findf (λ (func) 
                            (string=? name (hash-ref func 'name))) 
                          (hash-ref traces 'functions)))
      (cond
        [func 
          (define insns (hash-ref func 'instrs))
          insns]
        [else
          (hasheq 'op "nop")])]
    [else
      (list effect)]))

(define (inline-undef undef traces) 
  (list undef))

(define (inline-constant constant traces) 
  (list constant))

(define (inline-value value traces) 
  (define op (hash-ref value 'op))
  (define dest (hash-ref value 'dest))
  (define type (hash-ref value 'type))
  (match op
    [(? icall? op)
      (define name (first (hash-ref value 'funcs)))
      (define func (findf (λ (func) 
                            (string=? name (hash-ref func 'name))) 
                          (hash-ref traces 'functions)))
      (cond
        [func 
          (define insns (hash-ref func 'instrs))
          (define last-insn (if (empty? insns) (void) (last insns)))
          (if (equal? last-insn (hasheq 'op "const" 'type "bool" 'dest "cc-inl" 'value #t))
            (begin
                (append insns (list (hasheq 'op "id" 'type type 'args (list "cc-ret") 'dest dest)))
              )
            insns)]
        [else
          (hasheq 'op "nop")])]
    [else
      (list value)]))
