#lang racket
(require 
  "taint-invariants.rkt"
  "lang-util.rkt")

(provide
 taint-all-invariants)

(define (taint-all-invariants program)
  (define (fix f p)
    (let ([p* (f p)])
      (if (equal? p p*)
          p
          (fix f p*))))
  (fix (λ (p) (taint-invariants p)) program))
