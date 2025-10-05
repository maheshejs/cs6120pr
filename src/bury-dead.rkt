#lang racket
(require 
  "lang-util.rkt"
  "undead-analysis.rkt")

(provide
 bury-dead)

(define (bury-dead program)
  (define (fix f p)
    (let ([p* (f p)])
      (if (equal? p p*)
          p
          (fix f p*))))
  (fix (λ (p) (bury-prog (undead-analysis p))) program))

(define (bury-prog prog)
  (match prog
    [(hash 'functions functions) 
     (hash-update prog 'functions (curry map bury-func))]))

(define (bury-func func)
  (hash-update func 'bbs bury-bbs))

(define (bury-bbs bbs)
  (for/fold ([acc bbs]) 
    ([k (in-hash-keys bbs)]) 
    (hash-update acc k (lambda (lins-usts) (map car (filter bury-lin-ust? lins-usts))))))

(define (bury-lin-ust? lin-ust)
  (define lin (car lin-ust))
  (match lin
    [(? label? lin) #t]
    [(? insn? lin)  (bury-insn-ust? lin-ust)]))

(define (bury-insn-ust? insn-ust) 
  (define insn (car insn-ust))
  (define ust  (cdr insn-ust))
  (match insn
    [(? constant? constant) 
     (define dest (hash-ref constant 'dest))
     (set-member? ust dest)]
    [(? undef? undef) 
     (define dest (hash-ref undef 'dest))
     (set-member? ust dest)]
    [(? value? value) 
     (define dest (hash-ref value 'dest))
     (set-member? ust dest)]
    [(? effect? effect) 
     (define op (hash-ref effect 'op))
     (match op
       [(? nop? nop)
         #f]
       [else
         #t])]))
