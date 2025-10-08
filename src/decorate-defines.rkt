#lang racket
(require 
  racket/hash
  "lang-util.rkt")

(provide 
  decorate-defines)

(define (decorate-defines program)
  (match program
    [(hash 'functions functions) 
     (hash-update program 'functions (curry map decorate-func))]))

(define (decorate-func func) 
  (define bbs (hash-ref func 'bbs))
  (define args (hash-ref func 'args '()))
  (define defs
    (for/fold ([acc (for/hash ([type-dest (map (λ (arg) (cons (hash-ref arg 'type) (hash-ref arg 'name))) args)]) 
                      (values type-dest (set 'entry)))]) 
      ([(bb lins) (in-hash bbs)]) 
      (hash-union acc 
                  (for/fold ([acc (hash)]) 
                    ([lin lins]) 
                    (decorate-lin lin acc bb))
                  #:combine/key (λ (t-d s1 s2) (set-union s1 s2)))))
  (hash-set func 'defs defs))

(define (decorate-lin lin acc bb)
  (match lin
    [(? label? lin) (decorate-label lin acc bb)]
    [(? insn?  lin) (decorate-insn  lin acc bb)]))

(define (decorate-label label acc bb)
  acc)

(define (decorate-insn insn acc bb) 
  (match insn
    [(? effect?   insn) 
     (decorate-effect   insn acc bb)]
    [(? constant? insn) 
     (decorate-constant insn acc bb)]
    [(? undef? insn) 
     (decorate-undef    insn acc bb)]
    [(? value?    insn) 
     (decorate-value    insn acc bb)]))

(define (decorate-effect effect acc bb) 
  acc)

(define (decorate-constant constant acc bb)
  (define type-dest (cons (hash-ref constant 'type) (hash-ref constant 'dest)))
  (if (hash-has-key? acc type-dest)
    (hash-update acc type-dest (λ (bbs) (set-add bbs bb)))
    (hash-set acc type-dest (set bb))))

(define (decorate-undef undef acc bb)
  (define type-dest (cons (hash-ref undef 'type) (hash-ref undef 'dest)))
  (if (hash-has-key? acc type-dest)
    (hash-update acc type-dest (λ (bbs) (set-add bbs bb)))
    (hash-set acc type-dest (set bb))))

(define (decorate-value value acc bb)
  (define type-dest (cons (hash-ref value 'type) (hash-ref value 'dest)))
  (if (hash-has-key? acc type-dest)
    (hash-update acc type-dest (λ (bbs) (set-add bbs bb)))
    (hash-set acc type-dest (set bb))))
