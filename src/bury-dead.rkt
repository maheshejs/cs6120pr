#lang racket
(require 
  "lang-util.rkt"
  "undead-analysis.rkt")

(provide
 bury-dead)

(define (bury-dead program)
  (do ([prog-cur program] 
       [prog-next (bury-prog (undead-analysis program))]) 
    ((equal? prog-cur prog-next) prog-cur) 
    (set! prog-cur prog-next) 
    (set! prog-next (bury-prog (undead-analysis prog-next)))))

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
    [(? dest? insn) 
     (define dest (hash-ref insn 'dest))
     (set-member? ust dest)]
    [else  
     #t]))
