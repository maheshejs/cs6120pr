#lang racket
(require 
  data/queue
  graph 
  "dominator.rkt"
  "lang-util.rkt")

(provide
 hoist-phis)

(define (hoist-phis program)
  (match program
    [(hash 'functions functions) 
     (hash-update program 'functions (curry map hoist-func))]))

(define (hoist-func func)
  (define cfg  (hash-ref func 'cfg))
  (define df (get-dominator-frontier cfg))
  (define xs 
    (for/list ([bb (in-set (apply set-union (hash-values df)))]
               #:when (> (length (get-predecessors cfg bb)) 1))
      bb))
  (hash-update func 'bbs (λ (bbs) (hoist-bbs xs bbs cfg))))

(define (hoist-bbs xs bbs cfg)
  (for/fold ([acc bbs])
    ([x xs])
    (for/fold ([acc acc])
      ([(lin i) (in-indexed (hash-ref acc x))])
      (match lin 
        [(? label? label) 
         acc] 
        [(? insn? insn) 
         (match insn 
           [(? phi? phi)
            (for/fold ([acc (hash-update acc x (λ (lins) 
                                                 (list-update lins i (λ (lin) 
                                                                       (hoist-get phi)))))])
              ([y (get-predecessors cfg x)]
               [arg (hash-ref phi 'args)])
              (hash-update acc y (curry hoist-set phi arg)))] 
           [(? undef? undef) 
            acc] 
           [(? constant? constant) 
            acc] 
           [(? value? value) 
            acc] 
           [(? effect? effect) 
            acc])]))))

(define (hoist-get phi)
  (define type (hash-ref phi 'type))
  (define dest (hash-ref phi 'dest))
  (hasheq 'type type 'dest dest 'op "get"))

(define (hoist-set phi arg lins)
  (define dest (hash-ref phi 'dest))
  (define-values (first-lins second-lins) (splitf-at lins (λ (lin)
                                                            (if (insn? lin)
                                                              (not (terminator? (hash-ref lin 'op)))
                                                              #t))))
  (append first-lins (list (hasheq 'op "set" 'args (list dest arg))) second-lins))
