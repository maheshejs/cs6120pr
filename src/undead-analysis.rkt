#lang racket
(require 
  "lang-util.rkt")

(provide
 undead-analysis)

(define (undead-analysis program)
  (match program
    [(hash 'functions functions) 
     (hash-update program 'functions (curry map undead-func))]))

(define (undead-func func)
  (hash-update func 'bbs undead-bbs))

(define (undead-bbs bbs)
  (define max-k (apply max (hash-keys bbs)))
  (for/fold ([acc bbs]) 
    ([k (in-hash-keys bbs)]) 
    (if (= k max-k) 
      (hash-update acc k (lambda (lins) 
                           (map cons lins (cdr (foldr undead-lin (cons (set) empty) lins)))))
      (hash-update acc k (lambda (lins) 
                           (define ust-out 
                             (for/set ([lin lins] 
                                       #:when (dest? lin)) 
                               (hash-ref lin 'dest))) 
                           (map cons lins (cdr (foldr undead-lin (cons ust-out empty) lins))))))))

(define (undead-lin lin acc)
  (define ust-in (car acc))
  (match lin
    [(? label? lin) (cons ust-in acc)]
    [(? insn? lin)  (undead-insn lin acc)]))

(define (undead-insn insn acc) 
  (define ust-in (car acc))
  (define args (hash-ref insn 'args '()))
  (match insn
    [(? dest? insn) 
     (define dest (hash-ref insn 'dest))
     (define ust-out
       (for/fold ([acc (set-remove ust-in dest)]) 
         ([arg args])
         (set-add acc arg)))
     (cons ust-out acc)]
    [else  
     (define ust-out
       (for/fold ([acc ust-in]) 
         ([arg args])
         (set-add acc arg)))
     (cons ust-out acc)]))
